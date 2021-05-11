# Carga de librerías ------------------------------------------------------

# install.packages(c("tidyverse","tidytext","udpipe","epubr","stopwords","tm",
#                    "SnowballC","ggthemes","echarts4r","igraph","ggraph"))

library(tidyverse)
library(tidytext)
library(udpipe)
library(epubr)
library(stopwords)
library(tm)
library(SnowballC)
library(ggthemes)
library(echarts4r)
library(igraph)
library(ggraph)

# Carga de datos ----------------------------------------------------------

# Libro: El Padrino - Mario Puzzo
# Link a los datos: https://github.com/sXHugoXe/CursoPLNconR/tree/main/recursos/el_padrino.epub
raw_padrino = epub(file = "recursos/el_padrino.epub")
raw_padrino %>% head()
length(raw_padrino$data)
raw_padrino_text = raw_padrino$data[[1]]

# Palabras vacías
stopwords_es = stopwords(language = "es", source = "nltk")

# Modelo pre-entrenado de NLP para obtener metadata de texto
# udpipe_download_model("spanish", model_dir = "models/")
model_spanish = udpipe_load_model("models/spanish-gsd-ud-2.5-191206.udpipe")

# Preprocesamiento de datos -----------------------------------------------

# Tokenización y lematización
padrino_annotated = udpipe_annotate(model_spanish, raw_padrino_text$text) %>% 
  as_tibble()
padrino_annotated %>% select(token, lemma, upos) %>% head(20)

# Eliminación de stopwords, puntuación, números
padrino_annotated = padrino_annotated %>% 
  mutate(lemma = removePunctuation(lemma),
         lemma = removeNumbers(lemma),
         lemma = str_squish(lemma)) %>% 
  filter(!str_to_lower(lemma) %in% stopwords_es, lemma!="")
padrino_annotated %>% select(token, lemma, upos) %>% head(20)

# Eliminación de UPOS erróneos
padrino_annotated %>% select(upos) %>% table(useNA = "always")
padrino_annotated %>% 
  filter(upos %in% c("X","PUNCT")) %>% 
  select(token, lemma, upos) %>% head(10)
padrino_annotated = padrino_annotated %>% 
  filter(!upos %in% c("X","PUNCT"))

# Extra: Stemming
padrino_annotated = padrino_annotated %>% 
  mutate(stem = wordStem(token, "spanish"))
padrino_annotated %>% select(token, lemma, stem) %>% head(20)


# Visualizaciones ---------------------------------------------------------

# Frecuencia por etiqueta POS
padrino_annotated %>% 
  count(upos) %>% 
  ggplot()+
  geom_col(aes(x=reorder(upos,n),y=n,fill=upos), show.legend = F)+
  labs(x="Etiqueta POS", y="Frecuencia", title="Etiquetas POS")+
  coord_flip()+
  theme_wsj()

# Palabras más frecuentes por etiqueta POS
padrino_annotated %>% 
  filter(upos %in% c('NOUN','PROPN','VERB','ADJ')) %>% 
  count(upos, lemma) %>% 
  group_by(upos) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ggplot()+
  geom_col(aes(x=reorder_within(lemma,n,upos),y=n,fill=lemma), show.legend = F)+
  scale_x_reordered()+
  labs(x="Lemma", y="Frecuencia", title="Palabras - POS")+
  facet_wrap(vars(upos), scales = "free", ncol = 2)+
  coord_flip()+
  theme_wsj()

# Nubes de palabras -------------------------------------------------------

padrino_np = padrino_annotated %>% 
  filter(upos %in% c("NOUN","PROPN", "ADJ", "VERB")) %>%
  count(lemma, sort=T) %>% 
  filter(n > 100) %>% 
  e_color_range(n, color, colors=c("skyblue","blue","navyblue")) %>%
  e_charts() %>%
  e_cloud(lemma, n, color) %>% 
  e_tooltip()
padrino_np

# Análisis por bigramas ---------------------------------------------------

# Separación y limpieza en bigramas
padrino_bigramas = raw_padrino_text %>%
  unnest_tokens(bigrama, text, token = "ngrams", n = 2) %>% 
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  mutate(palabra1 = removePunctuation(palabra1),
         palabra1 = removeNumbers(palabra1),
         palabra1 = str_squish(palabra1),
         palabra2 = removePunctuation(palabra2),
         palabra2 = removeNumbers(palabra2),
         palabra2 = str_squish(palabra2)) %>%
  filter(!palabra1 %in% stopwords_es,
         palabra1!="",
         !palabra2 %in% stopwords_es,
         palabra2!="",)
padrino_bigramas %>% head(10)

# Frecuencia por bigramas
padrino_bigramas_frec = padrino_bigramas %>% 
  count(palabra1, palabra2, sort = TRUE) %>% 
  unite(bigrama, palabra1, palabra2, sep = " ")
padrino_bigramas_frec %>% head(10)

# Conexión entre palabras
padrino_bigrama_grafo = padrino_bigramas %>%
  count(palabra1, palabra2, sort = TRUE) %>% 
  filter(n >= 15) %>%
  graph_from_data_frame()
padrino_bigrama_grafo

# Redes de palabras
set.seed(42)
ggraph(padrino_bigrama_grafo, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_label(aes(label = name), vjust = 1, hjust = 1)

# Guardar resultado -------------------------------------------------------

saveRDS(padrino_annotated, file = "talleres/TallerXSeminarioSEE/prepared/padrino_annotated.RDS")
