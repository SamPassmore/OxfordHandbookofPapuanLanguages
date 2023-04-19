## UMAP projects

suppressPackageStartupMessages({
  library(umap)
})

#### Functions
jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

# Read in structural data
structuralvector_files = list.files('processed_data/', pattern = "vectors.csv", full.names = TRUE)
structural_vectors = lapply(structuralvector_files, read.csv, row.names = 1)

# list of papuan languages
papuan_languages = read.csv('processed_data/papuan_languages.csv')

## get UMAP projections
custom.config = umap.defaults
custom.config$metric = "manhattan"

lapply(structural_vectors, function(sv){
  # run UMAP on matrix
  out = umap(sv, config = umap.settings)
  
  # Save locations
})