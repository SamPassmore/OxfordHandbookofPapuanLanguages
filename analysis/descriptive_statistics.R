## This script constains useful descriptive statistics 
library(tidyverse)

papuan_languages = read_csv("processed_data/papuan_languages.csv")

cat("There are", nrow(papuan_languages), "papuan languages in Kinbank.\n")

# Isolates can be considered their own clade
papuan_languages$Clade_EF[papuan_languages$Family == "Isolate"] = papuan_languages$Name[papuan_languages$Family == "Isolate"]

# Change formatting so that all TNG langauges are labelled as such
tng_idx = str_detect(string = papuan_languages$Clade_EF, pattern = "^\\(TNG")
papuan_languages$Clade_EF[tng_idx] = "TNG"

cat("There are", n_distinct(papuan_languages$Clade_EF), "distinct clades across the Papuan languages in Kinbank.\n")

sort(table(papuan_languages$Clade_EF))

