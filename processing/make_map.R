## map of papuan languages

## build raster for New Guinea
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)
library(ggrepel)

## Get outline of New Guinea
new_guinea = sf::read_sf("processed_data/base_map.shp")

## Get data locations
papuan_languages = read.csv('processed_data/papuan_languages.csv')

## Get labal locations
label_data = papuan_languages %>% 
  filter(Name %in% c("Watam", "Ekari", "Nen", "Sibe"))

label_data$Name = recode(label_data$Name, Ekari = "Ekagi", Sibe = "Nagovisi")

p = ggplot(new_guinea) + 
  geom_sf(fill = "white") + 
  geom_point(data = papuan_languages, aes(x = Longitude, y = Latitude), shape = 21, fill = "black") + 
  geom_label_repel(data = label_data, aes(x = Longitude, y = Latitude, label = Name), min.segment.length = 0) + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  theme_minimal()

ggsave(filename = "map.png", plot = p)
