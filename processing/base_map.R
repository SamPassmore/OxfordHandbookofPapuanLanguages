## This script plots the New Guinea base map

suppressPackageStartupMessages({
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(dplyr)
  library(ggplot2)
})

#### Building a map of New Guinea ####

# Map of Indonesia
indonesia = ne_states(country = "Indonesia", returnclass = "sf")

# Extract West Papua
west_papua = indonesia[indonesia$name %in% c("Papua", "Papua Barat", "Maluku", "Nusa Tenggara Timur", "Maluku Utara"),]

# Map of PNG
png = ne_states(country = "Papua New Guinea", returnclass = "sf")
east_timor = ne_states(country = "East Timor", returnclass = "sf")

# Map of Solomons
solomons = ne_states(country = "Solomon Islands", returnclass = "sf")
solomons = solomons[solomons$longitude < 163,]

# Merge
new_guinea = rbind(west_papua, png, east_timor, solomons)

## Save
sf::write_sf(new_guinea, "processed_data/base_map.shp")
