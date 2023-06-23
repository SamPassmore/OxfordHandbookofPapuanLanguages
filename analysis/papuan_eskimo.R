## are there any eskimo patterns in Papuan langauges

library(kinbankr)

structuralvector_files = list.files('processed_data/', pattern = "vectors.csv", full.names = TRUE)
structural_vectors = lapply(structuralvector_files, read.csv, row.names = 1)

# list of papuan languages
papuan_languages = read.csv('processed_data/papuan_languages.csv')

papuan_vectors = lapply(structural_vectors, function(x) x[rownames(x) %in% papuan_languages$ID,])

parents_nuncles = structural_vectors[[1]]
sibs_cousins = structural_vectors[[3]]

## Eskimo patterns in Parents

# men
nuncle_vectors = get_structural_vectors(types,
                       languages = papuan_languages$ID, duplicates = "any") %>% 
  data.frame()
nuncle_patterns = apply(nuncle_vectors, 1, revert_vector) %>% 
  t() %>% 
  apply(., 1, paste, collapse='')

kinterms = read.csv('https://raw.githubusercontent.com/kinbank/kinbank/master/kinbank/cldf/forms.csv')

# View terms 
which(nuncle_patterns == 122)
types = c("mF", "mFeB", "mMeB", "mFyB")
types = c("mM", "mFeZ", "mMeZ", "mMyZ")
types = c("meB", "mFBeS", "mMBeS")
types = c("meZ", "mFBeD", "mMBeD")
language = "p_abuiabui1241"

kinterms %>% 
  filter(Language_ID == language) %>% 
  filter(Parameter_ID %in% types) %>% 
  select(Parameter_ID, Form)


