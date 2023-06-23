## UMAP projects

suppressPackageStartupMessages({
  library(umap)
  library(ggplot2)
  library(purrr)
  library(stringr)
  library(dplyr)
  library(ggforce)
  library(patchwork)
})

# set.seed(1123114)

# Read in structural data
structuralvector_files = list.files('processed_data/', pattern = "vectors.csv", full.names = TRUE)
structural_vectors = lapply(structuralvector_files, read.csv, row.names = 1)

# list of papuan languages
papuan_languages = read.csv('processed_data/papuan_languages.csv')
kinbank_languages = read.csv("submodules/kinbank/cldf/languages.csv")

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
# label language families

umap_df = left_join(umap_df, kinbank_languages, by = c("language" = "ID"))

umap_df = umap_df %>%
  mutate(., family = with(., case_when(
    Family == "Indo-European" ~ "Indo-European",
    Family == "Austronesian" ~ "Austronesian",
    Family == "Pama-Nyungan" ~ "Pama-Nyungan",
    language %in% papuan_languages$ID ~ "Papuan"
  )))


facet_names = 
  c(
  parentsandsibs_vectors.csv = "Parents &\n Parent's Siblings",
  sibling_vectors.csv = "Siblings",
  sibsandcousins_vectors.csv = "Siblings & Cousins", 
  "Indo-European" = "Indo-European",
  "Austronesian" = "Austronesian",
  "Pama-Nyungan" = "Pama-Nyungan",
  Papuan = "Papuan"
)

concavity = 20

umap_df$family = factor(umap_df$family, 
                        levels = c("Papuan", "Austronesian", "Indo-European",
                                   "Pama-Nyungan"))

p_base = ggplot() + 
  geom_hex(data = transform(umap_df, family = NULL), aes(x = X, y = Y)) + 
  geom_mark_hull(
    data = umap_df,
    aes(x = X, y = Y),
    concavity = concavity,
    color = "NA",
    fill = "#cae6d3",
    alpha = 0.5,
    expand = unit(2, "mm"),
    radius = unit(2, "mm")
  ) + 
  geom_jitter(data = umap_df[!is.na(umap_df$family),], aes(x = X, y = Y),  
              shape = 21, size = 2, width = 0.5, height = 0.5,
              col = "black", fill = 'white') + 
  scale_fill_continuous(low = "gray85", high = "black") +
  theme_classic(base_size = 20) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = "none") + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  facet_wrap(~id + family, labeller = as_labeller(facet_names), drop = T, ncol = 5)
p_base

## This plot includes a column of NA values. 
## I just cropped this for the publication version. 
# ggsave("design_space.png", plot = p_base, height = 290, width = 320, units = "mm")


## Functional diversity
types = unique(umap_df$id)
fr = list()
for(i in seq_along(types)){
  dd = umap_df %>% 
    filter(id == types[i])
  
  # subset to languages used
  kb_l = subset(kinbank_languages, kinbank_languages$ID %in% dd$language)
  
  # Make grouping matrix for language families
  grouping = table(kb_l$Family, kb_l$ID)
  
  # Reduce to the families of interest
  keep_families = c("Indo-European", "Austronesian", "Pama-Nyungan")
  grouping = grouping[rownames(grouping) %in% keep_families,]
  
  # Add a grouping for Papuan languages
  grouping = rbind(grouping, Papuan = ifelse(colnames(grouping) %in% papuan_languages$ID, 1, 0))
  # Add a grouping for Global diversity
  grouping = rbind(grouping, Global = 1)
  
  ## Return
  coords = dd[,c("X", "Y")]
  rownames(coords) = dd$language
  fr[[i]] = fundiversity::fd_fric(traits = coords, 
                             sp_com = grouping, 
                             stand = TRUE)
}
names(fr) = types
# fr_csv = lapply(fr, function(x) cbind(x[,1], round(x[,2], 2)))
fr_csv = purrr::map_df(fr, ~as.data.frame(.x), .id="id")
write.csv(fr_csv, "results/umap_functionalrichness.csv", row.names = FALSE)
