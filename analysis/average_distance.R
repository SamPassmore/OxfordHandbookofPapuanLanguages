## Structural distances

# Read in structural data
structuralvector_files = list.files('processed_data/', pattern = "vectors.csv", full.names = TRUE)
structural_vectors = lapply(structuralvector_files, read.csv, row.names = 1)

# list of papuan languages
papuan_languages = read.csv('processed_data/papuan_languages.csv')

# list of all languages
kinbank_languages = read.csv("submodules/kinbank/cldf/languages.csv")

# Make distance matrix between all languages
distance_matrices = lapply(structural_vectors, function(sv)
  as.matrix(dist(sv, method = "manhattan")))


## Find the average distance between all languages and between papuan languages 
summary_function = function(dist_mat, language_family, language_metadata){
  # Identify languages of interest
  idx = language_metadata$ID[language_metadata$Family == language_family]
  
  # Extract languages of interest from distance matrix
  dist_ss =  dist_mat[rownames(dist_mat) %in% idx, colnames(dist_mat) %in% idx]
  
  # Calculate the summary of distances
  c(summary(dist_ss[lower.tri(dist_ss)]), n = length(idx))
}

difference_summary = lapply(distance_matrices, function(dm){
  all = c(summary(dm[lower.tri(dm)]), n = nrow(dm))
  
  papuan_only = dm[rownames(dm) %in% papuan_languages$ID, colnames(dm)%in% papuan_languages$ID]
  papuan_sum = c(summary(papuan_only[lower.tri(papuan_only)]), n = sum(colnames(dm)%in% papuan_languages$ID))
  
  # Indo-European
  ie_summary = summary_function(dm, "Indo-European", kinbank_languages)
  
  # Austronesian
  an_summary = summary_function(dm, "Austronesian", kinbank_languages)
  
  # Pama-Nyungan
  pn_summary = summary_function(dm, "Pama-Nyungan", kinbank_languages)
  
  # Dravidian
  dv_summary = summary_function(dm, "Dravidian", kinbank_languages)
  
  
  rbind(all = all, papuan = papuan_sum, indoeuropean = ie_summary, austronesian = an_summary, 
       pamanyungan = pn_summary, dravidian = dv_summary)
})

n_comparisons = sapply(structural_vectors, ncol)

distance_comparison = purrr::map2(difference_summary, n_comparisons, function(x, y) {x / y})

names(distance_comparison) = structuralvector_files
names(difference_summary) = structuralvector_files

distance_comparison
difference_summary
