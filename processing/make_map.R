## map of papuan languages

## build raster for New Guinea
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)
library(ggrepel)

## Get outline of New Guinea
# Map of Indonesia
indonesia = ne_states(country = "Indonesia", returnclass = "sf")

# only include West Papua
west_papua = indonesia[indonesia$name %in% c("Papua", "Papua Barat", "Maluku", "Nusa Tenggara Timur", "Maluku Utara"),]

# Map of PNG
png = ne_states(country = "Papua New Guinea", returnclass = "sf")
east_timor = ne_states(country = "East Timor", returnclass = "sf")

# Map of Solomonds
solomons = ne_states(country = "Solomon Islands", returnclass = "sf")
solomons = solomons[solomons$longitude < 163,]

# Merge
new_guinea = rbind(west_papua, png, east_timor, solomons)

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
