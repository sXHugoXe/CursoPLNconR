# Carga de librerías ------------------------------------------------------
# install.packages(c("tidyverse","tidytext","udpipe","stopwords","tm",
#                    "ggthemes","ggrepel","text2vec","Rtsne","FactoMineR",
#                    "factoextra"))

library(tidyverse)
library(tidytext)
library(udpipe)
library(stopwords)
library(tm)
library(ggthemes)
library(ggrepel)
library(text2vec)
library(Rtsne)
library(FactoMineR)
library(factoextra)

# Carga de datos ----------------------------------------------------------

# Libro: El Padrino - Mario Puzzo (Procesado)
# Link a los datos: https://github.com/sXHugoXe/CursoPLNconR/blob/main/talleres/TallerXSeminarioSEE/prepared/padrino_annotated.RDS
padrino_annotated = readRDS("talleres/TallerXSeminarioSEE/prepared/padrino_annotated.RDS")

# Preparación para modelamiento -------------------------------------------

# Paso de lemmas a minúsculas
padrino_annotated = padrino_annotated %>% 
  mutate(lemma = str_to_lower(lemma))

# Vector de palabras
padrino_palabras = list(padrino_annotated %>% 
                          filter(upos %in% c("VERB","ADJ","NOUN","PROPN")) %>% 
                          pull(lemma) %>% 
                          tolower())
padrino_palabras %>% head()

# Word embeddings ---------------------------------------------------------

# Creamos un objeto iterable
i_token = itoken(iterable = padrino_palabras, progressbar = FALSE)

# Creamos el vocabulario a partir de la frecuencia de cada palabra
padrino_vocabulario = create_vocabulary(i_token)

# Frecuencias del vocabulario
padrino_vocabulario %>% 
  ggplot(aes(x=term_count))+
  geom_histogram()+
  ggtitle("Vocabulario")+
  scale_x_log10()+
  theme_wsj()

# Podamos el vocabulario, quedándonos con aquellos términos que tengan una frecuencia igual o superior a 5
padrino_vocabulario = prune_vocabulary(padrino_vocabulario, term_count_min = 5)

# Frecuencias del vocabulario podado
padrino_vocabulario %>% 
  ggplot(aes(x=term_count))+
  geom_histogram()+
  ggtitle("Vocabulario (podado)")+
  scale_x_log10()+
  theme_wsj()

# Vectorizamos el vocabulario creado
v_vectorizer = vocab_vectorizer(padrino_vocabulario)

# Creamos la matriz de co-ocurrencia de términos, definiendo una ventana de contexto
padrino_tcm = create_tcm(i_token, v_vectorizer, skip_grams_window = 10)
padrino_tcm %>% head()

# Inicializamos el modelo glove definiendo la dimensión del vector (50) y el tamaño del contexto (10)
glove = GlobalVectors$new(rank = 50, x_max = 10)

# Terminamos de entrenar el modelo con 100 iteraciones y un parámetro de tolerancia de 0.00001, paralelizando con los núcleos disponibles
padrino_wv_main = glove$fit_transform(padrino_tcm, n_iter = 100, 
                                      convergence_tol = 0.00001, n_threads = 4)

# Asignación del vector de contexto
padrino_wv_context = glove$components

# Creación de los vectores de palabras
padrino_word_vectors = padrino_wv_main + t(padrino_wv_context)
padrino_word_vectors[1:6, 1:6]

# Ejemplo de términos cercanos
michael = padrino_word_vectors["michael", , drop = F]
cos_sim_michael = sim2(x = padrino_word_vectors, y = michael, 
                       method = "cosine", norm = "l2")
head(sort(cos_sim_michael[,1], decreasing = T), 20)

tattaglia = padrino_word_vectors["tattaglia", , drop = F]
cos_sim_tattaglia = sim2(x = padrino_word_vectors, y = tattaglia,
                         method = "cosine", norm = "l2")
head(sort(cos_sim_tattaglia[,1], decreasing = T), 20)

# Reducción de dimensionalidad a través de t-SNE --------------------------

# Datos de entrenamiento como dataframe
padrino_train_df = padrino_word_vectors %>% 
  as.data.frame() %>%
  rownames_to_column("word")

# Entrenamiento de t-SNE
padrino_tsne = Rtsne(padrino_train_df[,-1], dims = 2, perplexity = 50, 
                     verbose=TRUE, max_iter = 1000)

# Visualización tsne ------------------------------------------------------

# Creación de un vector de palabras a visualizar
palabras = c(sort(cos_sim_michael[,1], decreasing = T) %>% head(20) %>% names(),
             sort(cos_sim_tattaglia[,1], decreasing = T) %>% head(20) %>% names())

# Definición de colores
colores = rainbow(length(unique(padrino_train_df$word)))
names(colores) = unique(padrino_train_df$word)

# Creación de dataframe con los resultados del algoritmo
padrino_tsne_df = data.frame(padrino_tsne$Y) %>%
  mutate(word = padrino_train_df$word,
         col = colores[padrino_train_df$word]) %>%
  left_join(padrino_vocabulario, by = c("word" = "term"))

# Gráfico
padrino_tsne_df %>% 
  filter(word %in% palabras) %>% 
  ggplot(aes(X1, X2)) +
  geom_label_repel(aes(X1, X2, label = word, color = col), 
                   size = 7, max.overlaps = 20, show.legend = F) +
  xlab("") + ylab("") +
  theme_wsj()

# Clustering de palabras --------------------------------------------------

# Matriz de similaridad
padrino_word_vectors["michael",]
padrino_sim = sim2(x = padrino_word_vectors, y = padrino_word_vectors,
                   method = "cosine", norm = "l2")
padrino_sim[1:5, 1:5]

# Número óptimo de clusters
fviz_nbclust(padrino_sim, nboot = 5, FUNcluster = kmeans)

# Estimación de clusters
padrino_km = kmeans(padrino_sim, centers = 2, nstart = 5)
padrino_km_df = tibble(word = padrino_sim %>% rownames(),
                       cluster = padrino_km$cluster)

# Visualización de clusters -----------------------------------------------

# Unión del resultado de t-SNE y la matriz beta tópicos
necronomicon_cluster_tsne_df = padrino_tsne_df %>% 
  left_join(padrino_km_df)

# Creación del gráfico
necronomicon_cluster_tsne_df %>% 
  filter((cluster==1 & term_count > 100)|
           (cluster==2 & term_count > 30)) %>% 
  ggplot(aes(X1, X2, label = word)) +
  geom_label_repel(size = 7, max.overlaps = 15, show.legend = F) +
  geom_point()+
  facet_wrap(vars(cluster), scales=  "free")+
  theme_wsj()
