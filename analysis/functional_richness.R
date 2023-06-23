suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(fundiversity)
  library(ggforce)
})

get_fr = function(structures, metric, dim){
 
  # make all data factors
  structures[structures == TRUE] = 1
  structures[structures == FALSE] = 0
  
  ## Calculate distance matrix
  dist_mat = cluster::daisy(structures[,-1], metric = metric, warnBin = FALSE)
  
  kinbank_mds = dist_mat %>% 
    cmdscale(eig = TRUE, k = dim)
  
  # subset to languages used
  kb_l = subset(kinbank_languages, kinbank_languages$ID %in% rownames(structures))
  
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
  fr = fundiversity::fd_fric(traits = kinbank_mds$points, 
                          sp_com = grouping, 
                          stand = TRUE)
  
  list(functional_richness = fr, mds = kinbank_mds)
}


## Load the data
structure_files = list.files('processed_data/', "vectors.csv", full.names = TRUE)
structural_vectors = sapply(structure_files, read.csv, row.names = 1)

kinbank_languages = read.csv('submodules/kinbank/cldf/languages.csv')

# Make Tok Pisin not IE
kinbank_languages$Family[kinbank_languages$Name == "Tok Pisin"] = ""

ignore_languages = 
  # These languages have overlaid kinship systems that makes it difficult to determine the appropriate structure
  c("p_bannonibann1247", "p_bariokelabitbari1288", "p_ifugawifugaoifug1247", "p_kulisusukuli1254",
  "p_maorimaor1246", "p_moronenemoro1287", "v_maorimaor1246", "p_riukbekatidayakbeka1241",
  "p_tolakitola1247", "p_toqabaqitatoab1237", "p_yakanyaka1277", "p_kogicogu1240",
  # These languages are all creoles which we have decided to remove
  "v_jamaicancreolejama1262", "p_kristangmalaccacreoleportugesemala1533", "p_torresstraitcreolebrokentorr1261")

# Load Papuan language data
papuan_languages = read.csv('processed_data/papuan_languages.csv')

# Calculate functional richness
structural_vectors = sapply(structural_vectors, function(x) x[!rownames(x) %in% ignore_languages,])

functional_richness = lapply(structural_vectors, function(x) get_fr(x, metric = "manhattan", dim = 2))
names(functional_richness) = basename(structure_files)

fr_scores = lapply(functional_richness, "[[", 1)

# fr_df = bind_rows(functional_richness, .id = "subset")

fr_df = lapply(functional_richness, function(x) x$mds$points)
fr_df = purrr::map_df(fr_df, ~as.data.frame(.x), .id="id")
fr_df$Language_ID = stringr::str_remove(rownames(fr_df), "\\.\\.\\.[0-9]+")
fr_df = left_join(fr_df, kinbank_languages, by = c("Language_ID" = "ID"))

fr_df = fr_df %>%
  mutate(., family = with(., case_when(
    Family == "Indo-European" ~ "Indo-European",
    Family == "Austronesian" ~ "Austronesian",
    Family == "Pama-Nyungan" ~ "Pama-Nyungan",
    Language_ID %in% papuan_languages$ID ~ "Papuan"
  )))


fr_df$family = factor(fr_df$family, 
                        levels = c("Papuan", "Austronesian", "Indo-European",
                                   "Pama-Nyungan"))

concavity = 20

facet_names = 
  c(
    parentsandsibs_vectors.csv = "Parents &\n Parent's Siblings",
    sibling_vectors.csv = "Siblings",
    sibsandcousins_vectors.csv = "Siblings & Cousins", 
    "Indo-European" = "Indo-European",
    "Austronesian" = "Austronesian",
    "Pama-Nyungan" = "Pama-Nyungan",
    Papuan = "Papuan",
    Global = "Global"
  )

p = ggplot() + 
  geom_hex(data = transform(fr_df, family = NULL), aes(x = V1, y = V2)) + 
  geom_mark_hull(
    data = fr_df,
    aes(x = V1, y = V2),
    concavity = concavity,
    color = "NA",
    fill = "#cae6d3",
    alpha = 0.5,
    expand = unit(2, "mm"),
    radius = unit(2, "mm")
  ) + geom_jitter(data = fr_df[!is.na(fr_df$family),], aes(x = V1, y = V2),  
                  shape = 21, size = 2, height = 1, width = 1,
                  col = "black", fill = 'white') + 
  scale_fill_continuous(low = "gray85", high = "black") +
  theme_classic(base_size = 20) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = "none") + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  facet_wrap(~id + family, drop = T, labeller = as_labeller(facet_names), ncol = 5, scales = "free")

## Simon's suggested changes
# No underlying global distribution
# Add global as a new facet
# make the green greyscale 
global_df = fr_df
global_df$family = "Global"
fr_df_global = fr_df %>% 
  filter(!is.na(family)) %>% 
  rbind(., global_df)

any(is.na(fr_df_global$family))

# axis limits
fr_df_global = fr_df_global %>%
  group_by(id) %>%
  mutate(ymax = max(V2),
         ymin = min(V2),
         xmax = max(V1),
         xmin = min(V1))

# order facets
fr_df_global$family = factor(fr_df_global$family, 
                             levels = c("Global", "Papuan", "Austronesian", 
                                        "Pama-Nyungan", "Indo-European"))

p = ggplot(data = fr_df_global) + 
  geom_mark_hull(
    aes(x = V1, y = V2),
    concavity = concavity,
    color = "NA",
    fill = "gray85",
    alpha = 0.5,
    expand = unit(2, "mm"),
    radius = unit(2, "mm")
  ) + geom_jitter(data = fr_df_global, aes(x = V1, y = V2),  
                  shape = 21, size = 2, height = 1, width = 1,
                  col = "black", fill = 'white') +
  theme_classic(base_size = 20) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = "none") + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  facet_wrap(~id + family, drop = T, labeller = as_labeller(facet_names), ncol = 5, scales = "free") + 
  geom_blank(aes(y = ymin)) + geom_blank(aes(y = ymax)) +
  geom_blank(aes(x = xmin)) + geom_blank(aes(x = xmax)) 

## table of FR
# lapply(fr, function(x) cbind(x[,1], round(x[,2], 2)))
ggsave(plot = p, filename = "design_space_mds_woOutliers.png", height = 290, width = 380, units = "mm")

fr_csv = purrr::map_df(fr_scores, ~as.data.frame(.x), .id="id")
write.csv(fr_csv, "results/mds_functionalrichness.csv", row.names = FALSE)
