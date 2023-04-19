## UMAP projects

suppressPackageStartupMessages({
  library(umap)
  library(ggplot2)
  library(purrr)
  library(stringr)
  library(dplyr)
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
custom.config$init = "random"
custom.config$n_neighbors = 3
custom.config$min_dist = 0.8

umap_output = lapply(structural_vectors, function(sv){
  # run UMAP on matrix
  o = umap(sv, config = custom.config)
  o$layout = data.frame(o$layout)
  colnames(o$layout) = c("X", "Y")

  o
})
umap_layouts = lapply(umap_output, "[[", 1)
names(umap_layouts) = basename(structuralvector_files)
umap_df = map_df(umap_layouts, ~as.data.frame(.x), .id="id")
umap_df$language = str_remove(rownames(umap_df), "\\.\\.\\.[0-9]+")
umap_df = left_join(umap_df, papuan_languages, by = c("language" = "ID"))

ggplot(data = umap_df, aes(x = X, y = Y)) + 
  geom_point(shape = 21) + 
  theme_classic() + 
  facet_wrap(~id)



## Plot
# uo = umap_output[[1]]
uo = umap(structural_vectors[[2]], config = custom.config)
uo$layout = data.frame(uo$layout)
colnames(uo$layout) = c("X", "Y")

### Testing UMAP settings
sv = structural_vectors[[1]] # 2 is siblings

n_neighbours_range = c(3, 5, 10, 15, 50, 100)
min_dist_range = c(0.01, 0.1, 0.25, 0.5, 0.8)

custom.config = umap.defaults
custom.config$metric = "manhattan"
custom.config$init = "random"
out = list()

func3 = function(x, y){
  custom.config$n_neighbors = x
  custom.config$min_dist = y
  
  o = umap(sv, config = custom.config)
  layout = data.frame(o$layout)

  colnames(layout) = c("X", "Y")
  layout
}
out = outer(n_neighbours_range, min_dist_range, Vectorize(func3, SIMPLIFY = FALSE))
dimnames(out) = list(paste0("neighbours_", n_neighbours_range), 
                     paste0("mindist_", min_dist_range))
out_list = lapply(out, function(x) x)
names(out_list) = outer(n_neighbours_range, min_dist_range, function(x, y){paste0("neighbours_", x, "_mindist", y)})

## Are the objects and names lining up as I would expect? 
# all(out[1,1][[1]] == out_list[[1]])
# all(out[2,1][[1]] == out_list[[2]])
# names(out)
# names(out_list)[1]
# names(out_list)[2]

out_df = purrr::map_df(out_list, ~as.data.frame(.x), .id="id")

ggplot(data = out_df, aes(x = X, y = Y)) + 
  geom_point() + 
  facet_wrap(~id, scales = "free")
