## This script will plot the location of Papuan langauges on a Map

suppressPackageStartupMessages({
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
})

## Get the outline of New Guinea from the previous script
new_guinea = sf::read_sf("processed_data/base_map.shp")

## Get location of languages
papuan_languages = read.csv('processed_data/papuan_languages.csv')

## Get the locations of labelled languages
label_data = papuan_languages %>% 
  filter(Name %in% c("Watam", "Ekari", "Nen", "Sibe"))

label_data$Name = recode(label_data$Name, Ekari = "Ekagi", Sibe = "Nagovisi")

## Plot the Map
p = ggplot(new_guinea) + 
  geom_sf(fill = "white") + 
  geom_point(data = papuan_languages, aes(x = Longitude, y = Latitude), shape = 21, fill = "black") + 
  geom_label_repel(data = label_data, aes(x = Longitude, y = Latitude, label = Name), min.segment.length = 0) + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  theme_minimal()

ggsave(filename = "map.png", plot = p)
