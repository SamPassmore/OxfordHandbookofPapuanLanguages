## This script plots the New Guinea base map

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)

## Maps
### Basemap
## map of papuan languages
## Get outline of New Guinea
# Map of Indonesia
indonesia = ne_states(country = "Indonesia", returnclass = "sf")

# only include West Papua
west_papua = indonesia[indonesia$name %in% c("Papua", "Papua Barat", "Maluku", "Nusa Tenggara Timur", "Maluku Utara"),]

# Map of PNG
png = ne_states(country = "Papua New Guinea", returnclass = "sf")
east_timor = ne_states(country = "East Timor", returnclass = "sf")

# Map of Solomons
solomons = ne_states(country = "Solomon Islands", returnclass = "sf")
solomons = solomons[solomons$longitude < 163,]

# Merge
new_guinea = rbind(west_papua, png, east_timor, solomons)

sf::write_sf(new_guinea, "processed_data/base_map.shp")