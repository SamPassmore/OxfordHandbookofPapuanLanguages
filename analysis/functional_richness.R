suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(fundiversity)
  library(ggforce)
  library(ggpattern)
})

#' Function to get Functional Richness for some subset of data
#'
#' @param structures Kinship structural vectors built in earlier scripts
#' @param metric Character. Showing what distance metric to use 
#' @param dim How many dimensions to calculate FR for. Numeric. Default 2.
#' @param kinbank_languages The language to analyse
#' @param grouping Groupings for the languages. Usually Language Family. 
#' @param of_interest Particular languages of interest to highlight. 
#'
#' @return A list containing the Functional Richness of the groupings, and the MDS output
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
# Details for ignoring are listed below. 
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

## Split Papuan languages into different facets
other_papuan = fr_df[fr_df$facets == "Papuan" & fr_df$within_facets %in% c("Yam", "Sepik", "North Halmahera"),] 
n_tng = fr_df[fr_df$facets == "Papuan" & fr_df$within_facets%in% c("Timor-Alor-Pantar", "Nuclear Trans New Guinea"),]

other_papuan$facets = "Papuan: Other Families"
n_tng$facets = "Papuan: Nuclear TNG"

## Add Papuan Facets to the main data
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

#### Make the plot ### 

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

## Save main plot
ggsave(plot = p, filename = "figures/design_space_mds_woOutliers.pdf", height = 297, width = 210, units = "mm")

fr_csv = purrr::map_df(fr_scores, ~as.data.frame(.x), .id="id")
write.csv(fr_csv, "results/mds_functionalrichness.csv", row.names = FALSE)