suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(fundiversity)
  library(ggforce)
  library(ggpattern)
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

# Put Papuan fixed up family names as the Kinbank family names 
kinbank_languages$Family[match(papuan_languages$ID, kinbank_languages$ID)] = papuan_languages$Family

# Calculate functional richness
structural_vectors = sapply(structural_vectors, function(x) x[!rownames(x) %in% ignore_languages,])

functional_richness = lapply(structural_vectors, function(x) get_fr(x, metric = "manhattan", dim = 2))
names(functional_richness) = basename(structure_files)

fr_scores = lapply(functional_richness, "[[", 1)

#### Plots ####

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
  ))) %>% 
  select(id, V1, V2, Language_ID, Name, Family, family)

global_df = fr_df
global_df$family = "Global"
global_df$Family = "Global"
fr_df_global = fr_df %>% 
  filter(!is.na(family)) %>% 
  rbind(., global_df)

any(is.na(fr_df_global$family) == TRUE)

# axis limits
fr_df_global = fr_df_global %>%
  group_by(id) %>%
  mutate(ymax = max(V2),
         ymin = min(V2),
         xmax = max(V1),
         xmin = min(V1))


## Extra facet
fr_df_global$family[fr_df_global$family == "Papuan" & fr_df_global$Family %in% c("Yam", "Timor-Alor-Pantar", "Sepik", "North Halmahera")] =
  "Papuan: Other Families"
fr_df_global$family[fr_df_global$family == "Papuan" & fr_df_global$Family == "Nuclear Trans New Guinea"] =
  "Papuan: N. TNG"

# relabel some columns because I am finding it confusing as is. 
fr_df_global$facet = fr_df_global$family
fr_df_global = fr_df_global %>% select(-family)

fr_df_global$within_facets = fr_df_global$Family
fr_df_global = fr_df_global %>% select(-Family)

# remove other papuan languages for now
fr_df_global %>% filter(facet == "Papuan") %>% group_by(Language_ID) %>% slice(1) %>% ungroup() %>% summarise(n()) # 50 languages
fr_df_global = fr_df_global %>% filter(facet != "Papuan")

table(fr_df_global$facet, fr_df_global$within_facets)

## make an additional hull for P-N without Kala Yagwa La
tmp = fr_df_global %>% 
  filter(Name != "Kala Lagaw Ya" & facet == "Pama-Nyungan")
tmp$within_facets = "Pama-Nyungan w/o KLY"
fr_df_global = rbind(fr_df_global, tmp)

## Highlight
fr_df_kly = fr_df_global %>% 
  filter(Name == "Kala Lagaw Ya" & facet == "Pama-Nyungan")

# order facets
fr_df_global$facet = factor(fr_df_global$facet, 
                            levels = c("Global", "Papuan: N. TNG", "Papuan: Other Families", "Austronesian", 
                                       "Pama-Nyungan", "Indo-European"))


# Arguments to make the graph pretty
concavity = 20
facet_names = 
  c(
    parentsandsibs_vectors.csv = "Parents & Parent's Siblings",
    sibling_vectors.csv = "Siblings",
    sibsandcousins_vectors.csv = "Siblings & Cousins", 
    "Indo-European" = "Indo-European",
    "Austronesian" = "Austronesian",
    "Pama-Nyungan" = "Pama-Nyungan",
    "Papuan: N. TNG" = "Papuan: N. TNG",
    "Papuan: Other Families" = "Papuan: Other Families",
    Global = "Global"
  )

p = ggplot(data = fr_df_global) + 
  geom_point(data = transform(fr_df_global, facet = NULL), aes(x = V1, y = V2), alpha = 0.1) +
  geom_mark_hull(
    data = fr_df_global,
    aes(x = V1, y = V2, fill = factor(within_facets)),
    concavity = concavity,
    color = "black",
    # fill = "gray85",
    alpha = 0.5,
    expand = unit(2, "mm"),
    radius = unit(2, "mm")) +
  geom_jitter(data = fr_df_global, aes(x = V1, y = V2),
                  shape = 21, size = 2, height = 1, width = 1,
                  col = "black", fill = 'white') +
  geom_label(data = fr_df_kly,
            aes(x = V1, y = V2),
            label = "Kala Lagaw Ya",
            vjust = 1.5, size = 2.5) + # this adds a label for KLY
  xlab(element_blank()) +
  ylab(element_blank()) +
  facet_wrap(~factor(facet) + id, drop = T, labeller = as_labeller(facet_names), ncol = 3, shrink = TRUE, scales = "free") + 
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = "bottom", legend.title = element_blank()) +
  geom_blank(aes(y = ymin)) + geom_blank(aes(y = ymax)) +
  geom_blank(aes(x = xmin)) + geom_blank(aes(x = xmax)) 

#p
## table of FR
# lapply(fr, function(x) cbind(x[,1], round(x[,2], 2)))
## Save main plot
ggsave(plot = p, filename = "design_space_mds_woOutliers.pdf", height = 297, width = 210, units = "mm")

fr_csv = purrr::map_df(fr_scores, ~as.data.frame(.x), .id="id")
write.csv(fr_csv, "results/mds_functionalrichness.csv", row.names = FALSE)

#### Within-Papuan language family variability #### 
fr_papuan = fr_df_global[fr_df_global$family == "Papuan",]

xx = data.frame(table(fr_papuan$Family, fr_papuan$id))
xx[xx$Freq >= 4,] %>% View()

fr_papuan$Language_Family = fr_papuan$Family

hull_df = fr_papuan[fr_papuan$Family %in% c("Yam", "Timor-Alor-Pantar", "Sepik", "North Halmahera"),]

hull_df2 = hull_df[hull_df$id == "parentsandsibs_vectors.csv",]

ggplot(data = hull_df2,
       aes(x = V1, y = V2, group = Language_Family)) + 
  geom_mark_hull() +
  geom_point(data = hull_df[hull_df$id == "parentsandsibs_vectors.csv", ], aes(x = V1, y = V2, shape = Language_Family)) 

p_papuan = ggplot(data = fr_papuan) + 
  geom_mark_hull(
    data = hull_df,
    aes(x = V1, y = V2, fill = Family),
    concavity = concavity,
    # color = "NA",
    alpha = 0.3,
    expand = unit(4, "mm"),
    radius = unit(4, "mm")) + 
  geom_jitter(data = fr_papuan, aes(x = V1, y = V2),
                  shape = 21, size = 2, height = 1, width = 1,
                  col = "black", fill = 'white') +
  theme_classic(base_size = 20) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = "right") +
  xlab(element_blank()) +
  ylab(element_blank()) +
  facet_wrap(~id + family, drop = T, labeller = as_labeller(facet_names), ncol = 5, scales = "free") + 
  geom_blank(aes(y = ymin)) + geom_blank(aes(y = ymax)) +
  geom_blank(aes(x = xmin)) + geom_blank(aes(x = xmax)) 

p_papuan
