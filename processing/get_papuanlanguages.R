# This script will get all Papuan languages in Kinbank
suppressPackageStartupMessages({
  library(dplyr)  
})

# Load kinbank data
kinbank_languages = read.csv('submodules/kinbank/cldf/languages.csv')

# Coordinates for New Guinea Map
lat_min = -15
lat_max = 5.1
long_min = 120
long_max = 170

## Subset to langauges that are "Papuan"
papuan_languages = kinbank_languages %>% 
  dplyr::filter(Longitude > long_min & Longitude < long_max & # Only Languages within the New Guinea Square
                  Latitude > lat_min & Latitude < lat_max) %>% 
  dplyr::filter(!Family %in% c("Austronesian", "Pama-Nyungan", "Indo-European")) %>% # Not Austronesian or Pama-Nyungan langauges
  dplyr::filter(Macroarea == "Papunesia" | Macroarea == "") # Not languages in Australia

# manually add kora1295
papuan_languages = rbind(papuan_languages, kinbank_languages[kinbank_languages$Glottocode == "kora1295",])
# manually add lat long
papuan_languages$Latitude[papuan_languages$Glottocode == "kora1295"] = -9.07
papuan_languages$Longitude[papuan_languages$Glottocode == "kora1295"] = 149.267

## Some languages have no language family in the database, but they should.
## I add them here. 
# If a language is on its own - then we name the family Isolate. 

papuan_languages$Family[papuan_languages$ID == "p_abuiabui1241"] = "Timor-Alor-Pantar"
papuan_languages$Family[papuan_languages$ID == "p_adangadan1251"] = "Timor-Alor-Pantar"
papuan_languages$Family[papuan_languages$ID == "p_blagarblag1240"] = "Timor-Alor-Pantar"
papuan_languages$Family[papuan_languages$ID == "p_dunaduna1248"] = "Isolate"
papuan_languages$Family[papuan_languages$ID == "p_endepapuanewguineaende1235"] = "Pahoturi"
papuan_languages$Family[papuan_languages$ID == "p_idinucl1597"] = "Pahoturi"
papuan_languages$Family[papuan_languages$ID == "p_kalamangkara1499"] = "West Bomberai"
papuan_languages$Family[papuan_languages$ID == "p_kamangkama1365"]  = "Timor-Alor-Pantar"
papuan_languages$Family[papuan_languages$ID == "p_kuotkuot1243"] = "Isolate"
papuan_languages$Family[papuan_languages$ID == "p_kwomakwom1262"] = "Sepik"
papuan_languages$Family[papuan_languages$ID == "p_maklewmakl1246"] = "Bulaka River"
papuan_languages$Family[papuan_languages$ID == "p_malimali1284"] = "Baining"
papuan_languages$Family[papuan_languages$ID == "p_marindnucl1622"] = "Anim"
papuan_languages$Family[papuan_languages$ID == "p_meriammeri1244"] = "Eastern Trans-Fly"
papuan_languages$Family[papuan_languages$ID == "p_meyahmeya1236"] = "East Bird's Head"
papuan_languages$Family[papuan_languages$ID == "p_namanama1266"] = "Yam"
papuan_languages$Family[papuan_languages$ID == "p_nambonamb1293"] = "Yam"
papuan_languages$Family[papuan_languages$ID == "p_nemeneme1244"] = "Yam"
papuan_languages$Family[papuan_languages$ID == "p_nennenn1238"] = "Yam"
papuan_languages$Family[papuan_languages$ID == "p_pagupagu1249"] = "North Halmahera"
papuan_languages$Family[papuan_languages$ID == "p_riantanarian1263"] = "Kolopom"
papuan_languages$Family[papuan_languages$ID == "p_rotokasroto1249"] = "North Bougainville"
papuan_languages$Family[papuan_languages$ID == "p_sougbmani1235"] = "East Bird's Head"
papuan_languages$Family[papuan_languages$ID == "p_sukisuki1245"] = "Suki-Gogodala"
papuan_languages$Family[papuan_languages$ID == "p_teiwateiw1235"] = "Timor-Alor-Pantar" 
papuan_languages$Family[papuan_languages$ID == "p_tidoreversion1tido1248"] = "North Halmahera"
papuan_languages$Family[papuan_languages$ID == "p_tidoreversion2tido1248a"] = "North Halmahera"
papuan_languages$Family[papuan_languages$ID == "p_wartathuntaigunt1241"] = "Yam"
papuan_languages$Family[papuan_languages$ID == "p_wersingwers1238"] = "Timor-Alor-Pantar" 
papuan_languages$Family[papuan_languages$ID == "p_westernpantarlamm1241"] = "Timor-Alor-Pantar" 
papuan_languages$Family[papuan_languages$ID == "p_wipiwipi1242"] = "Eastern Trans-Fly"
papuan_languages$Family[papuan_languages$ID == "p_yelmekyelm1242"] = "Bulaka River"

# Grouping Bougainville languages is maybe too far (but we only have data on four languages total)
papuan_languages$Family[papuan_languages$Family == "North Bougainville"] = "Bougainville"
papuan_languages$Family[papuan_languages$Family == "South Bougainville"] = "Bougainville"

papuan_languages$Family[papuan_languages$Family == "Nuclear Torricelli"] = "Torricelli"
papuan_languages$Family[papuan_languages$Family == "Bogia"] = "Torricelli"

# Tor-Kwerba
papuan_languages$Family[papuan_languages$Family == "Greater Kwerba"] = "Tor-Kwerba"
papuan_languages$Family[papuan_languages$Family == "Tor-Orya"] = "Tor-Kwerba"

# We want to use the oxford handbook clade families so we add those here
# Match languages to clades
papuan_clades = read.csv('data/clades.csv')

papuan_languages = left_join(papuan_languages,
                             papuan_clades,
                             by = c("Family" = "Clade_HH2022"))

# Some Manual alignment
papuan_languages$Clade_EF[papuan_languages$Family == "Kaure-Narau"] = "Kaure"
papuan_languages$Clade_EF[papuan_languages$Family == "Goilalan"] = "(TNG; Goilalan)"
papuan_languages$Clade_EF[papuan_languages$Family == "Maybrat-Karon"] = "Maybrat"
papuan_languages$Clade_EF[papuan_languages$Family == "Yele"] = "Yélî-Dnye"

# checks that missing languages are now in
all(c("soge1235", "kora1295", "yali1257") %in% papuan_languages$Glottocode)

## How many languages?
n_languages = nrow(papuan_languages)
cat("There are", n_languages, "Papuan languages.\n")

## How many clades?
n_clades = n_distinct(papuan_languages$Family)
cat("There are", n_clades, "different families.\n")

write.csv(papuan_languages, "processed_data/papuan_languages.csv", row.names = FALSE)


