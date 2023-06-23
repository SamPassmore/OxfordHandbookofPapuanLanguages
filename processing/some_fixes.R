## Fixing some kinship systems
library(dplyr)
library(stringr)
library(kinbankr)

structure_files = list.files('processed_data/', "vectors.csv", full.names = TRUE)
structural_vectors = sapply(structure_files, read.csv, row.names = 1)

kinterms = read.csv("submodules/kinbank/cldf/forms.csv")

## Siblings 
sibs = structural_vectors[[2]]

sib_terms = outer(c("m", "f"), c("eB", "eZ", "yB", "yZ"), paste0)
sib_terms = c(sib_terms[1,], sib_terms[2,])

# Adang
adang = kinterms %>% 
  filter(Parameter_ID %in% sib_terms) %>% 
  filter(str_detect(Language_ID, "adan1251")) %>% 
  filter(Form != "-uding") 
adang = adang[match(sib_terms, adang$Parameter_ID),]

adang_terms = adang$Form
names(adang_terms) = adang$Parameter_ID

adang_vector = kinbankr::get_vector(adang$Form)

possible_names = outer(names(adang_terms), names(adang_terms), paste0)
vector_names = possible_names[lower.tri(possible_names)]
names(adang_vector) = vector_names

all(vector_names %in% colnames(sibs))
adang_vector = adang_vector[colnames(sibs)]

sibs[str_detect(rownames(sibs), "adan1251"),] = adang_vector

# Momu-Fas
Momu = kinterms %>% 
  filter(Parameter_ID %in% sib_terms) %>% 
  filter(str_detect(Language_ID, "fass1245")) %>% 
  filter(Form != "menyo") 
Momu = Momu[match(sib_terms, Momu$Parameter_ID),]
nrow(Momu) == 8
Momu_terms = Momu$Form
names(Momu_terms) = Momu$Parameter_ID

Momu_vector = kinbankr::get_vector(Momu$Form)

possible_names = outer(names(Momu_terms), names(Momu_terms), paste0)
vector_names = possible_names[lower.tri(possible_names)]
names(Momu_vector) = vector_names

all(vector_names %in% colnames(sibs))
Momu_vector = Momu_vector[colnames(sibs)]

sibs[str_detect(rownames(sibs), "fass1245"),] = Momu_vector

# Sawi
sawi = kinterms %>% 
  filter(Parameter_ID %in% sib_terms) %>% 
  filter(str_detect(Language_ID, "sawi1257")) %>% 
  filter(Form != "aesom") 
sawi = sawi[match(sib_terms, sawi$Parameter_ID),]
nrow(sawi) == 8
sawi_terms = sawi$Form
names(sawi_terms) = sawi$Parameter_ID

sawi_vector = kinbankr::get_vector(sawi$Form)

possible_names = outer(names(sawi_terms), names(sawi_terms), paste0)
vector_names = possible_names[lower.tri(possible_names)]
names(sawi_vector) = vector_names

all(vector_names %in% colnames(sibs))
sawi_vector = sawi_vector[colnames(sibs)]

sibs[str_detect(rownames(sibs), "sawi1257"),] = sawi_vector

# Teiwa
teiwa = kinterms %>% 
  filter(Parameter_ID %in% sib_terms) %>% 
  filter(str_detect(Language_ID, "teiw1235")) %>% 
  filter(Form != "-ianqai") 
teiwa = teiwa[match(sib_terms, teiwa$Parameter_ID),]
nrow(teiwa) == 8
teiwa_terms = teiwa$Form
names(teiwa_terms) = teiwa$Parameter_ID

teiwa_vector = kinbankr::get_vector(teiwa$Form)

possible_names = outer(names(teiwa_terms), names(teiwa_terms), paste0)
vector_names = possible_names[lower.tri(possible_names)]
names(teiwa_vector) = vector_names

all(vector_names %in% colnames(sibs))
teiwa_vector = teiwa_vector[colnames(sibs)]

sibs[str_detect(rownames(sibs), "teiw1235"),] = teiwa_vector

write.csv(sibs, "processed_data/sibling_vectors.csv")

# Sibs and cousins
sib_cousins = structural_vectors[[3]]

sibs_andcousins = outer(c("m", "f"), c("eB", "eZ", "yB", "yZ",  # siblings
                                       "MBeS", "MByS", "MBeD", "MByD", # mother's brother's children
                                       "MZeS", "MZyS", "MZeD", "MZyD", # mother's sister's children
                                       "FBeS", "FByS", "FBeD", "FByD", # father's brother's children
                                       "FZeS", "FZyS", "FZeD", "FZyD"), paste0) # father's sister's children
sibs_andcousins = c(sibs_andcousins[1,], sibs_andcousins[2,])

# Kyaka
Kyaka = kinterms %>% 
  filter(Parameter_ID %in% sibs_andcousins) %>% 
  filter(str_detect(Language_ID, "kyak1244")) %>% 
  filter(!Form %in% c("kaingi", "yangonge", "pemalenge", "kakinyi"))
# kainigi is a generalise cousin term, but all cousins can be referred to by sibling terms. 
# The remaining terms are alternative spellings of sibling terms. 
nrow(Kyaka) == length(sibs_andcousins)

Kyaka = Kyaka[match(sibs_andcousins, Kyaka$Parameter_ID),]
Kyaka_terms = Kyaka$Form
names(Kyaka_terms) = Kyaka$Parameter_ID

Kyaka_vector = kinbankr::get_vector(Kyaka$Form)

possible_names = outer(names(Kyaka_terms), names(Kyaka_terms), paste0)
vector_names = possible_names[lower.tri(possible_names)]
names(Kyaka_vector) = vector_names

all(vector_names %in% colnames(sib_cousins))
Kyaka_vector = Kyaka_vector[colnames(sib_cousins)]
all(names(Kyaka_vector) == colnames(sib_cousins))
sib_cousins[str_detect(rownames(sib_cousins), "kyak1244"),] = Kyaka_vector

# Abui
abui = kinterms %>% 
  filter(Parameter_ID %in% sibs_andcousins) %>% 
  filter(str_detect(Language_ID, "abui1241")) %>% 
  filter(!Form %in% c("-moknehi", "-ura", "-mayol fala", "-neng fala"))
View(abui)
# kainigi is a generalise cousin term, but all cousins can be referred to by sibling terms. 
# The remaining terms are alternative spellings of sibling terms. 
nrow(abui) == length(sibs_andcousins)

abui = abui[match(sibs_andcousins, abui$Parameter_ID),]
abui_terms = abui$Form
names(abui_terms) = abui$Parameter_ID

abui_vector = kinbankr::get_vector(abui$Form)

possible_names = outer(names(abui_terms), names(abui_terms), paste0)
vector_names = possible_names[lower.tri(possible_names)]
names(abui_vector) = vector_names

all(vector_names %in% colnames(sib_cousins))
abui_vector = abui_vector[colnames(sib_cousins)]
all(names(abui_vector) == colnames(sib_cousins))
sib_cousins[str_detect(rownames(sib_cousins), "kyak1244"),] = abui_vector

write.csv(sib_cousins, "processed_data/sibsandcousins_vectors.csv")
