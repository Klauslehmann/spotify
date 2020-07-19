###########################################
#VISUALIZAR INFORMACIÓN DE BANDAS CHILENAS#
###########################################

library(tidyverse)

#Cargar datos
load(file = "data/bandas_chile.RData")
load(file = "data/bandas_filter.RData")
load(file = "data/album_info.RData")


#Cargar script muy largo con bandas que hay que sacar 
source("scripts/sacar_bandas.R", encoding = "UTF-8")

#Promedio de bandas chilenas
promedio_bandas <- album_info %>% 
  mutate(duracion = duration_ms / 1000 / 60) %>% 
  group_by(artist_name) %>% 
  summarize_at(vars(danceability, loudness, energy, valence, tempo, duracion), .funs = mean) %>% 
  slice(1:203) %>% 
  filter(!artist_name %in% eliminar)  


ggplot(promedio_bandas, aes(x = valence, y = energy)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = artist_name)) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(limits = c(0.2, 0.9)) +
  scale_y_continuous(limits = c(0.2, 1)) +
  labs(title = "Clasificación de bandas chilenas según energía y valencia") +
  annotate("text", x = 0.2, y = 0.2, label = "Depre", size = 10, color = "black") +
  annotate("text", x = 0.2, y = 0.95, label = "Furioso", size = 10, color = "black") +
  annotate("text", x = 0.8, y = 0.2, label = "Relajado", size = 10, color = "black") +
  annotate("text", x = 0.8, y = 0.95, label = "Alegre", size = 10, color = "black") +
  theme(text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 21),
        plot.subtitle =  element_text(hjust = 0.5, size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 17))


ggsave("data/bandas.png", width = 13, height = 7)


#Comparar canciones de algunas bandas
comparar <- album_info %>% 
  filter(artist_name == "Fiskales Ad-Hok" | artist_name == "Guachupé" |
           artist_name == "Camila Moreno") %>% 
  mutate(duracion = duration_ms / 1000 / 60) 

ggplot(comparar, aes(x = valence, y = energy, group = artist_name, color = artist_name)) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  labs(title = "Clasificación de canciones de bandas chilenas",
       subtitle = "Camila Moreno, Fiskales y Guachupé",
       x = "valence (negativo-positivo)",
       y = "energy",
       caption = "Fuente: Spotify (Klaus Lehmann)") +
  annotate("text", x = 0.05, y = 0.1, label = "Depre", size = 8, color = "black") +
  annotate("text", x = 0.05, y = 1, label = "Furioso", size = 8, color = "black") +
  annotate("text", x = 0.9, y = 0.1, label = "Relajado", size = 8, color = "black") +
  annotate("text", x = 0.9, y = 1, label = "Alegre", size = 8, color = "black") +
  theme(text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 21),
        plot.subtitle =  element_text(hjust = 0.5, size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 17))

ggsave("data/tres_bandas.png", width = 13, height = 7)
