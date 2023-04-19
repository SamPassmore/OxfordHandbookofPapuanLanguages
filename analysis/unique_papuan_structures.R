# This script looks at kinship terminology variability compared to global variability 

# read in data about all languages
structuralvector_files = list.files('processed_data/', pattern = "vectors.csv", full.names = TRUE)
structural_vectors = lapply(structuralvector_files, read.csv, row.names = 1)

# list of papuan languages
papuan_languages = read.csv('processed_data/papuan_languages.csv')

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

  c(n_papuan = n_papuan_languages, n_papuan_structures = n_papuan_structures, 
    n_languages = n_languages, n_structures = n_structures,
    random_structures = randomsample_mean, random_structures_sd = randomsample_prettysd)
})
unique_papuan = t(unique_papuan)
rownames(unique_papuan) = c("Parents and Parent's Siblings", "Siblings", "Siblings and Cousins")

write.csv(unique_papuan, "results/papuan_structures.csv")
