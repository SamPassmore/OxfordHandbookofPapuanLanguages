# This script plots syncretisms 

library(sf)
library(ggplot2)

new_guinea = read_sf("processed_data/base_map.shp")

papuan_syncretisms = read.csv('processed_data/papuan_syncretisms.csv')

# Omaha
ggplot(data = new_guinea) + 
  geom_sf(fill = "white") + 
  geom_point(data = papuan_syncretisms, aes(x = Longitude, y = Latitude, fill = factor(structure.omaha), col = Family), 
             shape = 21) 
  

# Omaha
ggplot(data = new_guinea) + 
  geom_sf(fill = "white") + 
  geom_point(data = papuan_syncretisms, aes(x = Longitude, y = Latitude, fill = factor(structure.siblings), col = Family), 
             shape = 21) 

# BM
papuan_syncretisms$BMF = ifelse(papuan_syncretisms$structure == 112, 1, 0)
papuan_syncretisms$BMM = ifelse(papuan_syncretisms$structure.mother == 112, 1, 0)

papuan_syncretisms$BMCF = ifelse(papuan_syncretisms$structure.cousinF == 112, 1, 0)
papuan_syncretisms$BMCM = ifelse(papuan_syncretisms$structure.cousinM == 112, 1, 0)

sum(table(papuan_syncretisms$BMCM, papuan_syncretisms$BMF))
sum(table(papuan_syncretisms$BMCF, papuan_syncretisms$BMM))


ggplot(data = new_guinea) + 
  geom_sf(fill = "white") + 
  geom_point(data = papuan_syncretisms, aes(x = Longitude, y = Latitude, col = factor(BMF), shape = factor(BMM)))
