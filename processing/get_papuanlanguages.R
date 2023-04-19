# This script will get all Papuan languages in Kinbank
library(dplyr)

kinbank_languages = read.csv('submodules/kinbank/cldf/languages.csv')

# square off languages in New Guinea
lat_min = -15
lat_max = 5
long_min = 120
long_max = 170


papuan_languages = kinbank_languages %>% 
  dplyr::filter(Longitude > long_min & Longitude < long_max & # Only Languages within the New Guinea Square
                  Latitude > lat_min & Latitude < lat_max) %>% 
  dplyr::filter(!Family %in% c("Austronesian", "Pama-Nyungan", "Indo-European")) %>% # Not Austronesian or Pama-Nyungan langauges
  dplyr::filter(Macroarea == "Papunesia") # Not languages in Australia

## How many languages?
n_languages = nrow(papuan_languages)
cat("There are", n_languages, "Papuan languages.\n")

## How many clades?
n_clades = n_distinct(papuan_languages$Family)
cat("There are", n_clades, "different families.\n")

write.csv(papuan_languages, "processed_data/papuan_languages.csv")


