# This script looks at kinship terminology variability compared to global variability 

# read in data about all languages
structuralvector_files = list.files('processed_data/', pattern = "vectors.csv", full.names = TRUE)
structural_vectors = lapply(structuralvector_files, read.csv, row.names = 1)

# list of papuan languages
papuan_languages = read.csv('processed_data/papuan_languages.csv')
kinbank_languages = read.csv('submodules/kinbank/cldf/languages.csv')

# list of nearby Language family languages
ie_languages = kinbank_languages$ID[kinbank_languages$Family == "Indo-European"]
an_languages = kinbank_languages$ID[kinbank_languages$Family == "Austronesian"]
pn_languages = kinbank_languages$ID[kinbank_languages$Family == "Pama-Nyungan"]

## For each kin subset, 
# 1. ask how many unique structures are there in the Papuan languages
# 2. ask how many unique structures there are in total
# 3. ask how many unique structures we would expect in a random sample of n_papuan languages

iter = 1000
unique_papuan = sapply(structural_vectors, function(x){
  # How many Papuan languages?
  papuan_vectors = subset(x, rownames(x) %in% papuan_languages$ID)
  n_papuan_languages = nrow(papuan_vectors)
  
  # How many unique kinship structures
  n_papuan_structures = sum(!duplicated(papuan_vectors))
  
  ## How many Austronesian, Indo-European and Pama-Nyungan languages 
  n_ie_languages = sum(rownames(x) %in% ie_languages)
  n_an_languages = sum(rownames(x) %in% an_languages)
  n_pn_languages = sum(rownames(x) %in% pn_languages)
  
  ## How many Austronesian, Indo-European and Pama-Nyungan structures
  n_ie_structures = sum(!duplicated(x[rownames(x) %in% ie_languages,]))
  n_an_structures = sum(!duplicated(x[rownames(x) %in% an_languages,]))
  n_pn_structures = sum(!duplicated(x[rownames(x) %in% pn_languages,]))
  
  # How many Languages total
  n_languages = nrow(x)
  
  # How many unique structures total
  n_structures = sum(!duplicated(x))
  
  # How many unique structures for a random sample of languages (the same size as the Papuan sample)
  random_sample = sapply(1:iter, function(i){
    sample_languages = sample(x = 1:n_languages, size = n_papuan_languages, replace = FALSE)
    sum(!duplicated(x[sample_languages,]))
  })
  
  randomsample_mean = mean(random_sample)
  randomsample_sd = sd(random_sample)
  randomsample_prettysd = paste0(round(randomsample_mean - randomsample_sd, 2), " - ",
                                 round(randomsample_mean + randomsample_sd, 2))

  c(
    n_papuan = n_papuan_languages, n_papuan_structures = n_papuan_structures,
    n_languages = n_languages, n_structures = n_structures,
    n_ie_languages, n_ie_structures,
    n_an_languages, n_an_structures,
    n_pn_languages, n_pn_structures,
    random_structures = randomsample_mean,
    random_structures_sd = randomsample_prettysd
  )
})
colnames(unique_papuan) = c("Parents and Parent's Siblings", "Siblings", "Siblings and Cousins")
rownames(unique_papuan) = c("N Papuan", "Papuan Structures",
                            "N Total Languages", "N Total Structures",
                            "N IE", "N IE Structures",
                            "N AN", "N AN Structures",
                            "N PN", "N PN Structures",
                            "Random Structures", "Random SD")

write.csv(unique_papuan, "results/papuan_structures.csv")

## Calculate ratios
ratios = rbind(
  as.numeric(unique_papuan["N Papuan", ]) / as.numeric(unique_papuan["Papuan Structures", ]),
  as.numeric(unique_papuan["N Total Languages", ]) / as.numeric(unique_papuan["N Total Structures", ]),
  as.numeric(unique_papuan["N IE", ]) / as.numeric(unique_papuan["N IE Structures", ]),
  as.numeric(unique_papuan["N AN", ]) / as.numeric(unique_papuan["N AN Structures", ]),
  as.numeric(unique_papuan["N PN", ]) / as.numeric(unique_papuan["N PN Structures", ])
)
dimnames(ratios) = list(c("Papuan", "Total", "IE", "AN", "PN"),
                        colnames(unique_papuan))

round(ratios, 3)
