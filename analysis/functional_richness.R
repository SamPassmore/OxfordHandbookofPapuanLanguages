suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(fundiversity)
  library(ggforce)
  library(ggpattern)
})

get_fr = function(structures,
                  metric,
                  dim,
                  kinbank_languages = kinbank_languages,
                  grouping = "facets", 
                  of_interest = NULL
){
 
  # make all data factors
  structures[structures == TRUE] = 1
  structures[structures == FALSE] = 0
  
  ## Calculate distance matrix
  dist_mat = cluster::daisy(structures[,-1], metric = metric, warnBin = FALSE)
  
  kinbank_mds = dist_mat %>% 
    cmdscale(eig = TRUE, k = dim)
  
  # subset to languages used
  kb_l = subset(kinbank_languages, kinbank_languages$Language_ID %in% rownames(structures))
  
  # Make grouping matrix for language families
  grouping = table(kb_l[,grouping], kb_l$Language_ID)
  
  if(!is.null(of_interest)){
    grouping = grouping[rownames(grouping) %in% of_interest,]
  }
  
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

# Load Papuan language data
papuan_languages = read.csv('processed_data/papuan_languages.csv')

# Put Papuan fixed up family names as the Kinbank family names 
kinbank_languages$Family[match(papuan_languages$ID, kinbank_languages$ID)] = papuan_languages$Family

# remove languages that are problematic
ignore_languages = 
  # These languages have overlaid kinship systems that makes it difficult to determine the appropriate structure
  c("p_bannonibann1247", "p_bariokelabitbari1288", "p_ifugawifugaoifug1247", "p_kulisusukuli1254",
    "p_maorimaor1246", "p_moronenemoro1287", "v_maorimaor1246", "p_riukbekatidayakbeka1241",
    "p_tolakitola1247", "p_toqabaqitatoab1237", "p_yakanyaka1277", "p_kogicogu1240",
    # These languages are all creoles which we have decided to remove
    "v_jamaicancreolejama1262", "p_kristangmalaccacreoleportugesemala1533", "p_torresstraitcreolebrokentorr1261")

structural_vectors = sapply(structural_vectors, function(x) x[!rownames(x) %in% ignore_languages,])

#### Data ####
fr_df = kinbank_languages

fr_df = fr_df %>%
  mutate(., family = with(., case_when(
    Family == "Indo-European" ~ "Indo-European",
    Family == "Austronesian" ~ "Austronesian",
    Family == "Pama-Nyungan" ~ "Pama-Nyungan",
    ID %in% papuan_languages$ID ~ "Papuan"
  ))) %>% 
  select(Language_ID = ID, Name, Family, family) %>%
  rename(within_facets = Family,
         facets = family)

## Split Papuan facets
other_papuan = fr_df[fr_df$facets == "Papuan" & fr_df$within_facets %in% c("Yam", "Sepik", "North Halmahera"),] 
n_tng = fr_df[fr_df$facets == "Papuan" & fr_df$within_facets%in% c("Timor-Alor-Pantar", "Nuclear Trans New Guinea"),]

other_papuan$facets = "Papuan: Other Families"
n_tng$facets = "Papuan: Nuclear TNG"

## Add Papuan Facets
fr_df = rbind(fr_df, n_tng, other_papuan)

# Calculate functional richness
functional_richness = lapply(structural_vectors, function(x) get_fr(x, metric = "manhattan", dim = 2, kinbank_languages = fr_df))
names(functional_richness) = basename(structure_files)

fr_scores = lapply(functional_richness, "[[", 1)

## Calculate fr for within facets
functional_richness_wnfacets = lapply(structural_vectors, function(x)
  get_fr(
    x,
    metric = "manhattan",
    dim = 2,
    kinbank_languages = fr_df,
    grouping = "within_facets",
    of_interest = c("Yam", "Sepik", "North Halmahera", "Timor-Alor-Pantar", "Nuclear Trans New Guinea")
  ))


fr_points = lapply(functional_richness, function(x) x$mds$points)
fr_points = purrr::map_df(fr_points, ~as.data.frame(.x), .id="id")
fr_points$Language_ID = stringr::str_remove(rownames(fr_points), "\\.\\.\\.[0-9]+")
fr_df = left_join(fr_points, fr_df, by = c("Language_ID" = "Language_ID"), relationship = "many-to-many")

# Remove other within_facets
fr_df$within_facets[!fr_df$within_facets %in% c("Yam", "Timor-Alor-Pantar", "Sepik", "North Halmahera", "Austronesian", "Indo-European", "Pama-Nyungan", "Nuclear Trans New Guinea")] = NA
fr_df$within_facets[fr_df$facets == "Papuan"] = "Papuan"

## make an additional hull for P-N without Kala Yagwa La
tmp = fr_df %>% 
  filter(Name != "Kala Lagaw Ya" & facets == "Pama-Nyungan")
tmp$within_facets = "Pama-Nyungan w/o KLY"
fr_df = rbind(fr_df, tmp)

## Highlight KLY within P-N
fr_df_kly = fr_df %>% 
  filter(Name == "Kala Lagaw Ya" & facets == "Pama-Nyungan")

## Make a special point for Kala Lagaw Ya in Papuan
tmp2 = fr_df %>% 
  filter(Name == "Kala Lagaw Ya" & facets == "Pama-Nyungan")
tmp3 = tmp2
tmp2$facets = "Papuan"
tmp2$within_facets = "Papuan"

tmp2 = rbind(tmp2, tmp3)

# order facets
fr_df$facets = factor(fr_df$facets, 
                            levels = c("Papuan", "Papuan: Nuclear TNG", "Papuan: Other Families", "Austronesian", 
                                       "Pama-Nyungan", "Indo-European"))

#### Arguments to make the graph pretty ####

# axis limits
fr_df = fr_df %>%
  group_by(id) %>%
  mutate(ymax = max(V2),
         ymin = min(V2),
         xmax = max(V1),
         xmin = min(V1))

concavity = 20
facet_names = 
  c(
    parentsandsibs_vectors.csv = "Parents & Parent's Siblings",
    sibling_vectors.csv = "Siblings",
    sibsandcousins_vectors.csv = "Siblings & Cousins", 
    "Indo-European" = "Indo-European",
    "Austronesian" = "Austronesian",
    "Pama-Nyungan" = "Pama-Nyungan",
    "Papuan: Nuclear TNG" = "Papuan: TNG",
    "Papuan: Other Families" = "Papuan: Other Families",
    Papuan = "All Papuan"
  )

idx = which(fr_df$within_facets == "Yam" & fr_df$facets == "Papuan: Other Families" & fr_df$id == "sibling_vectors.csv")
fr_df$V1[idx] = fr_df$V1[idx] + rnorm(4)

p = ggplot() + 
  geom_jitter(data = transform(fr_df[!is.na(fr_df$facets),], facets = NULL), aes(x = V1, y = V2), height = 1, width = 1, alpha = 0.1) +
  geom_mark_hull(
    data = fr_df[!is.na(fr_df$facets),],
    aes(x = V1, y = V2, group = factor(within_facets), fill = factor(within_facets)),
    concavity = concavity,
    color = "black",
    alpha = 0.5,
    expand = unit(2, "mm"),
    radius = unit(2, "mm")) +
  geom_jitter(data = fr_df[!is.na(fr_df$facets),], aes(x = V1, y = V2),
                  shape = 21, size = 2, height = 0.5, width = 0.5,
                  col = "black", fill = 'white') +
  geom_label(data = fr_df_kly,
            aes(x = V1, y = V2),
            label = "Kala Lagaw Ya",
            vjust = 1.5, size = 2.5) + # this adds a label for KLY
  geom_point(data = tmp2, aes(x = V1, y = V2), shape = 23, fill = "red", size = 4) +
  # geom_point(data = fr_df[!is.na(fr_df$facets) & fr_df$within_facets == "Yam",], aes(x = V1, y = V2), col = "green") + 
  xlab(element_blank()) +
  ylab(element_blank()) +
  facet_wrap(~factor(facets) + id, drop = TRUE, labeller = as_labeller(facet_names), ncol = 3, scales = "free") +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.position = "bottom", legend.title = element_blank()) +
  geom_blank(data = fr_df[!is.na(fr_df$facets),], aes(y = ymin)) + geom_blank(data = fr_df[!is.na(fr_df$facets),], aes(y = ymax)) +
  geom_blank(data = fr_df[!is.na(fr_df$facets),], aes(x = xmin)) + geom_blank(data = fr_df[!is.na(fr_df$facets),], aes(x = xmax))

#p
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
