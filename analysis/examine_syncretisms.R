## Comparisons of interest

suppressPackageStartupMessages({
  library(kinbankr)
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(patchwork)
  library(fastDummies)
})

papuan_languages = read.csv('processed_data/papuan_languages.csv')
# kinbank language metadata
data("languages")

## functions
process_function = function(kin_types, papuan_languages){
  # get vectors
  structural_vectors = kinbankr::get_structural_vectors(kin_types, duplicates ="random", method = "binary")
  
  # Revert to structures
  structures = t(apply(structural_vectors, 1, function(f) kinbankr::revert_vector(unlist(f), nmes = kin_types)))
  structures = data.frame(structures)
  
  structures$papuan = ifelse(rownames(structures) %in% papuan_languages$ID, 1, 0)
  structures$structure = apply(structures[,seq_along(kin_types)], 1, paste, collapse = "")
  
  summary_table = table(structures$structure, structures$papuan)
  proportions = prop.table(summary_table, margin = 2)
  
  totals = colSums(summary_table)
  
  # Get chi-square test
  chi2 = apply(summary_table, 1, chi_test_wrapper, totals = totals)
  p_stars = ifelse(chi2[2,] < 0.1, "*",
                      ifelse(chi2[2,] < 0.01, "**",
                             ifelse(chi2[2,] < 0.001, "***", "")))
  chi2 = rbind(chi2, p_stars)
  rownames(chi2) = c("Chi-square", "p-value", "p-star") 
  
  # order by papuan proportions
  proportions = proportions[order(proportions[,2], decreasing = TRUE),]
  
  proportions = data.frame(structure = rownames(proportions),
                           `Non-Papuan` = round(proportions[,1], 4),
                           Papuan = round(proportions[,2], 4))
  
  list(summary = proportions, structures = structures, chi2 = chi2)
}

make_proportionplot = function(data, title, labels, counts){
  data$name = gsub(x = data$name, pattern = "\\.", replacement = "-")
  
  ggplot(data = data, aes(x = structure, y = value)) + 
    geom_bar(aes(fill = name), stat="identity", position = "dodge") + 
    ylim(c(0, 0.6)) + 
    ggtitle(title) + 
    xlab(element_blank()) + 
    ylab(element_blank()) + 
    scale_fill_grey() + 
    scale_x_discrete(labels = labels) + 
    theme_classic(base_size = 16) + 
    annotate("label", x = 4.5, y=0.55, label = counts, hjust = 1, label.r = unit(0, "pt")) +
    theme(legend.title=element_blank())
}

chi_test_wrapper = function(summary_table, totals){
  c2 = chisq.test(rbind(summary_table, totals - summary_table), simulate.p.value = TRUE)
  c(c2$statistic, c2$p.value)
}
  

# Parent / Nuncle males
## F = FB = MB
## F != FB = MB
## F = FB != MB
## F != FB != MB

fn_types = c("mF", "mFeB", "mMeB")
fathernuncle_structures = process_function(fn_types, papuan_languages = papuan_languages)
fathernuncle_structures$structures$Language_ID = rownames(fathernuncle_structures$structures)

# The most common papuan structure is 11122 or (F = FeB = MeB) != (FyB = MyB)
plot_father = data.frame(fathernuncle_structures$summary)
plot_long_father = pivot_longer(plot_father, cols = !structure) 

plot_long_father = plot_long_father %>% 
  filter(value != 0) %>% 
  filter(structure != 121)

f_counts = table(fathernuncle_structures$structures$papuan)

chi_father = fathernuncle_structures$chi2["p-star",]

p_father = make_proportionplot(
  plot_long_father,
  "Father & Uncles",
  labels = c("F = FB = MB", "F = FB ≠ MB",
             "F ≠ FB = MB", "F ≠ FB ≠ MB"),
  counts = paste("Non-Papuan = ", f_counts[1], "\nPapuan = ", f_counts[2], sep = "")) + 
  ggpubr::geom_bracket(
    xmin = 1.75, xmax = 2.25,
    y.position = 0.6, label = chi_father["112"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 2.75, xmax = 3.25,
    y.position = 0.34, label = chi_father["122"],
    tip.length = 0.04
  )

## Modelling 
df = fathernuncle_structures$structures
dummies = cbind(papuan = df$papuan, fastDummies::dummy_cols(df$structure))

fit = glm(.data_112 ~ papuan, data = dummies, family = "poisson")
summary(fit)

## Aunts 
fn_types = c("mM", "mMeZ", "mFeZ")
aunt_structures = process_function(fn_types, papuan_languages = papuan_languages)
aunt_structures$structures$Language_ID = rownames(aunt_structures$structures)

# The most common papuan structure is 11122 or (F = FeB = MeB) != (FyB = MyB)
plot_mother = data.frame(aunt_structures$summary)
plot_long_mother = pivot_longer(plot_mother, cols = !structure) 

plot_long_mother = plot_long_mother %>% 
  filter(value != 0) %>% 
  filter(structure != 121)

m_counts = paste(table(aunt_structures$structures$papuan), collapse = "\n")

chi_mother = aunt_structures$chi2["p-star",]

p_mother = make_proportionplot(
  plot_long_mother,
  "Mother & Aunts",
  labels = c("M = MZ = FZ", "M = MZ ≠ FZ",
             "M ≠ MZ = FZ", "M ≠ MZ ≠ FZ"),
  counts = m_counts
) +
  ggpubr::geom_bracket(
    xmin = 1.75, xmax = 2.25,
    y.position = 0.51, label = chi_mother["112"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 2.75, xmax = 3.25,
    y.position = 0.38, label = chi_mother["122"],
    tip.length = 0.04
  )

# Cousins
cousin_types = c("meB", "mFBeS", "mFZeS")
cousinM_structures = process_function(cousin_types, papuan_languages = papuan_languages)
cousinM_structures$structures$Language_ID = rownames(cousinM_structures$structures)

## Cousin organisation (Males)
plot_cousinM = data.frame(cousinM_structures$summary)
plot_long_cousinM = pivot_longer(plot_cousinM, cols = !structure) 

plot_long_cousinM = plot_long_cousinM %>% 
  filter(value != 0) %>% 
  filter(structure != 121)

cm_counts = paste(table(cousinM_structures$structures$papuan), collapse = "\n")

chi_cmM = cousinM_structures$chi2["p-star",]

p_cousinM = make_proportionplot(
  plot_long_cousinM,
  "Brother & Male cousins",
  labels = c("B = FBS = MBS", "B = FBS ≠ MBS",
             "B ≠ FBS = MBS", "B ≠ FBS ≠ MBS"),
  counts = cm_counts
) + ylim(c(0, 0.67)) +
  ggpubr::geom_bracket(
    xmin = 1.75, xmax = 2.25,
    y.position = 0.65, label = chi_cmM["112"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 2.75, xmax = 3.25,
    y.position = 0.52, label = chi_cmM["122"],
    tip.length = 0.04
  )

# Female cousins
cousin_types = c("meZ", "mMZeD", "mFZeD")
cousinF_structures = process_function(cousin_types, papuan_languages = papuan_languages)
cousinF_structures$structures$Language_ID = rownames(cousinF_structures$structures)

plot_cousinF = data.frame(cousinF_structures$summary)
plot_long_cousinF = pivot_longer(plot_cousinF, cols = !structure) 

plot_long_cousinF = plot_long_cousinF %>% 
  filter(value != 0) %>% 
  filter(structure != 121)

cf_counts = paste(table(cousinF_structures$structures$papuan), collapse = "\n")

chi_cmF = cousinF_structures$chi2["p-star",]

p_cousinF = make_proportionplot(
  plot_long_cousinF,
  "Sister & Female cousins",
  labels = c("Z = MZD = FZD", "Z = MZD ≠ FZD",
             "Z ≠ MZD = FZD", "Z ≠ MZD ≠ FZD"),
  counts = cf_counts
)  +
  ggpubr::geom_bracket(
    xmin = 1.75, xmax = 2.25,
    y.position = 0.49, label = chi_cmF["112"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 2.75, xmax = 3.25,
    y.position = 0.533, label = chi_cmF["122"],
    tip.length = 0.04
  )


## Other notable differences 
# Omaha Cousins
omaha_types = c("mMeB", "mMyB", "mMBeS", "mMByS")
omaha_structures = process_function(omaha_types, papuan_languages = papuan_languages)
omaha_structures$structures$Language_ID = rownames(omaha_structures$structures)

plot_omaha = data.frame(omaha_structures$summary)
plot_omaha_long = pivot_longer(plot_omaha, cols = !structure) 

plot_omaha_long = plot_omaha_long %>% 
  filter(value != 0) %>% 
  filter(structure == 1111)

#### Sibling organisation 
siblings_types = c("meB", "myB", "meZ", "myZ")
sibling_structures = process_function(siblings_types, papuan_languages = papuan_languages)
sibling_structures$structures$Language_ID = rownames(sibling_structures$structures)

plot_sibs = data.frame(sibling_structures$summary)
plot_sibs_long = pivot_longer(plot_sibs, cols = !structure) 

plot_sibs_long = plot_sibs_long %>% 
  filter(value != 0) %>% 
  filter(structure %in% c(1233))

# Papuan families
sibling_pap = left_join(sibling_structures$structures[sibling_structures$structures$papuan == 1,], papuan_languages,
                        by = c("Language_ID" = "ID")) %>% 
  filter(structure %in% c(1233))

## Statistical test
df = sibling_structures$structures
df$is_1111 = ifelse(df$structure == 1111, 1, 0)

summary(glm(papuan ~ factor(structure), data = df, family = "poisson"))
exp(0.6)

## Grandparent reciprocals 
grandM_types = c("mFF", "mMF", "mSS", "mDS")
grandM_structures = process_function(grandM_types, papuan_languages = papuan_languages)
grandM_structures$structures$Language_ID = rownames(grandM_structures$structures)

plot_grandM = data.frame(grandM_structures$summary)
plot_grandM_long = pivot_longer(plot_grandM, cols = !structure) 

plot_grandM_long = plot_grandM_long %>% 
  filter(value != 0) %>% 
  filter(structure %in% c(1111))

# Papuan families
grandM_pap = left_join(grandM_structures$structures[grandM_structures$structures$papuan == 1,], papuan_languages,
                        by = c("Language_ID" = "ID")) %>% 
  filter(structure == 1111)

table(grandM_pap$structure, grandM_pap$Clade_EF)

grandF_types = c("mFM", "mMM", "mSD", "mDD")
grandF_structures = process_function(grandF_types, papuan_languages = papuan_languages)
grandF_structures$structures$Language_ID = rownames(grandF_structures$structures)

plot_grandF = data.frame(grandF_structures$summary)
plot_grandF_long = pivot_longer(plot_grandF, cols = !structure) 

plot_grandF_long = plot_grandF_long %>% 
  filter(value != 0) %>% 
  filter(structure %in% c(1111))

# join other interesting stats
interesting_df = list(plot_omaha_long, plot_sibs_long, plot_grandM_long, plot_grandF_long)
names(interesting_df) = c("Omaha", "Siblings", "GrandkinM", "GrandkinF")

interesting_df = bind_rows(interesting_df, .id = "type")
interesting_df$structure.pretty = factor(c("MB = MBS", "MB = MBS", # Omaha labels
                                    "eB ≠ yB ≠ eZ = yZ", "eB ≠ yB ≠ eZ = yZ", # Sibling org. 1
                                    "eB = yB = eZ = yZ", "eB = yB = eZ = yZ", # Sibling org. 2
                                    "FF = MF = SS = DS", "FF = MF = SS = DS", # Grandkin M
                                    "MM = FM = SD = DD", "MM = FM = SD = DD" # Grandkin F
                                    ), 
                                    levels = c("eB ≠ yB ≠ eZ = yZ", "eB = yB = eZ = yZ",
                                               "FF = MF = SS = DS", "MM = FM = SD = DD",
                                               "MB = MBS"))

om_counts = paste(table(omaha_structures$structures$papuan), collapse = "\n")
sib_counts = paste(table(sibling_structures$structures$papuan), collapse = "\n")
gm_counts = paste(table(grandM_structures$structures$papuan), collapse = "\n")
gf_counts = paste(table(grandF_structures$structures$papuan), collapse = "\n")

interesting_df$name = gsub(x = interesting_df$name, pattern = "\\.", replacement = "-")

p_interesting = ggplot(data = interesting_df, aes(x = structure.pretty, y = value)) + 
  geom_bar(aes(fill = name), stat="identity", position = "dodge") + 
  ylim(c(0, 0.6)) + 
  ggtitle("Other features") + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  scale_fill_grey() + 
  theme_classic(base_size = 16) + 
  annotate("text", x = 1.5, y=0.53, label = "Siblings", hjust = 0.5, size = 7) +
  annotate("text", x = 3.5, y=0.53, label = "Grandkin", hjust = 0.5, size = 7) +
  annotate("text", x = 5.0, y=0.53, label = "Matrilineal\nSkewing", hjust = 0.5, size = 7) +
  annotate("label", x = 2.4, y=0.42, label = sib_counts, hjust = 1, label.r = unit(0, "pt")) +
  annotate("label", x = 2.6, y=0.42, label = gm_counts, hjust = 0, label.r = unit(0, "pt")) +
  annotate("label", x = 4.4, y=0.42, label = gf_counts, hjust = 1, label.r = unit(0, "pt")) +
  annotate("label", x = 5.4, y=0.42, label = om_counts, hjust = 1, label.r = unit(0, "pt")) +
  geom_vline(xintercept = 2.5) + 
  geom_vline(xintercept = 4.5) + 
  theme(legend.title=element_blank()) +
  ggpubr::geom_bracket(
    xmin = .75, xmax = 1.25,
    y.position = 0.328, label = sibling_structures$chi2[3,"1233"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 1.75, xmax = 2.25,
    y.position = 0.189, label = sibling_structures$chi2[3,"1111"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 2.75, xmax = 3.25,
    y.position = 0.411, label = grandM_structures$chi2[3,"1111"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 3.75, xmax = 4.25,
    y.position = 0.352, label = grandF_structures$chi2[3,"1111"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 4.75, xmax = 5.25,
    y.position = 0.212, label = omaha_structures$chi2[3,"1111"],
    tip.length = 0.04
  )

# Frequency graphs
(p_father + p_mother) / (p_cousinM + p_cousinF) / p_interesting +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
grid::grid.draw(grid::textGrob("Proportion", x = 0.007, rot = 90))
ggsave("proportions_plot.png", height = 300, units = "mm")

#### Bifurcate merging patterns by Language family ####
## Do languages with bifurcate merging in parents have bifurcate merging in other parts. 

# merge all data into one big file
languages_structures = left_join(languages, fathernuncle_structures$structures, by = c("ID" = "Language_ID"), suffix = c("", ".father")) %>% 
  left_join(., aunt_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".mother")) %>% 
  left_join(., cousinM_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".cousinM")) %>% 
  left_join(., cousinF_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".cousinF")) %>% 
  left_join(., sibling_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".siblings")) %>% 
  left_join(., grandM_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".grandM")) %>% 
  left_join(., grandF_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".grandF")) %>% 
  left_join(., omaha_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".omaha"))


# Simple tables
## Father
table(languages_structures$structure, languages_structures$papuan)
## Mother
table(languages_structures$structure.mother, languages_structures$papuan)
## Male Cousins
table(languages_structures$structure.cousinM, languages_structures$papuan)
## Female cousins
table(languages_structures$structure.cousinF, languages_structures$papuan)

# Number of languages with BM in F and other subsets
# Rows is F structures, Columns is other structure
table(languages_structures$structure, languages_structures$structure.mother, languages_structures$papuan)
table(languages_structures$structure, languages_structures$structure.cousinM, languages_structures$papuan)
table(languages_structures$structure, languages_structures$structure.cousinF, languages_structures$papuan)

table(languages_structures$structure.cousinM, languages_structures$structure.cousinF, languages_structures$papuan)
table(languages_structures$structure.cousinM, languages_structures$structure.mother, languages_structures$papuan)
table(languages_structures$structure.grandM, languages_structures$papuan)

## Coherency of BM in Parents generation
languages_structures$bm_inconsistent = rowSums(cbind(languages_structures$structure == 112,
                                        languages_structures$structure.mother == 112)) == 1

languages_structures$bm_consistent = rowSums(cbind(languages_structures$structure == 112,
                                                     languages_structures$structure.mother == 112)) == 2

## BM rel proportion table
papuan_structures = languages_structures[languages_structures$papuan == 1,]
vars = c("structure", "structure.mother", "structure.cousinM", "structure.cousinF")
tt = matrix(NA, ncol = 4, nrow = 4)
tt[1,] =  colSums(papuan_structures[,vars] == 112, na.rm = TRUE) / sum(papuan_structures$structure == 112, na.rm = TRUE)
tt[2,] =  colSums(papuan_structures[,vars] == 112, na.rm = TRUE) / sum(papuan_structures$structure.mother == 112, na.rm = TRUE)
tt[3,] =  colSums(papuan_structures[,vars] == 112, na.rm = TRUE) / sum(papuan_structures$structure.cousinM == 112, na.rm = TRUE)
tt[4,] =  colSums(papuan_structures[,vars] == 112, na.rm = TRUE) / sum(papuan_structures$structure.cousinF == 112, na.rm = TRUE)
dimnames(tt) = list(c("MP", "FP", "MC", "FM"), c("MP", "FP", "MC", "FM"))
round(tt, 2)

## Coherency of BM in cousin generation
languages_structures$bmc_inconsistent = rowSums(cbind(languages_structures$structure.cousinM == 112,
                                                     languages_structures$structure.cousinF == 112)) == 1

languages_structures$bmc_consistent = rowSums(cbind(languages_structures$structure.cousinM == 112,
                                                   languages_structures$structure.cousinF == 112)) == 2

table(languages_structures$bmc_inconsistent, languages_structures$papuan)
table(languages_structures$bmc_consistent, languages_structures$papuan)



# How many Papuan languages have BM in all subsets
sum(languages_structures$papuan == 1 &
  languages_structures$structure == 112 & 
      languages_structures$structure.mother == 112 & 
      languages_structures$structure.cousinM == 112 & 
      languages_structures$structure.cousinF == 112, na.rm = TRUE)

# How many papuan languages have BM in G+1
sum(languages_structures$papuan == 1 &
      languages_structures$structure == 112 & 
      languages_structures$structure.mother == 112, na.rm = TRUE)

# How many papuan languages have BM in G0
sum(languages_structures$papuan == 1 &
      languages_structures$structure.cousinM == 112 & 
      languages_structures$structure.cousinF == 112, na.rm = TRUE)


# How many Papuan languages do we have data for all subsets
sum(languages_structures$papuan == 1 &
  !is.na(languages_structures$structure) & 
      !is.na(languages_structures$structure.mother) & 
      !is.na(languages_structures$structure.cousinM) &
      !is.na(languages_structures$structure.cousinF))

# How many Papuan languages do we have data on G+1
sum(languages_structures$papuan == 1 &
      !is.na(languages_structures$structure) & 
      !is.na(languages_structures$structure.mother))

# How many Papuan languages do we have data on G0
sum(languages_structures$papuan == 1 &
      !is.na(languages_structures$structure.cousinM) &
      !is.na(languages_structures$structure.cousinF))

# Male G+1 & G0
sum(languages_structures$papuan == 1 &
      !is.na(languages_structures$structure.cousinM) &
      !is.na(languages_structures$structure))

table(languages_structures$structure.cousinM,
      languages_structures$structure,
      languages_structures$papuan)

table(languages_structures$structure.cousinF,
      languages_structures$structure.mother,
      languages_structures$papuan)

papuan_idx = languages_structures$papuan == 1

## Papuan structures
papuan_structure = left_join(papuan_languages, fathernuncle_structures$structures, by = c("ID" = "Language_ID"), suffix = c("", ".father")) %>% 
  left_join(., aunt_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".mother")) %>% 
  left_join(., cousinM_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".cousinM")) %>% 
  left_join(., cousinF_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".cousinF")) %>% 
  left_join(., sibling_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".siblings")) %>% 
  left_join(., grandM_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".grandM")) %>% 
  left_join(., grandF_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".grandF")) %>% 
  left_join(., omaha_structures$structures, by = c("ID" = "Language_ID", "papuan"), suffix = c("", ".omaha"))

# Father/Nuncle LF
x_f = table(papuan_structure$Family,
      papuan_structure$structure)
sum(x_f[,"112"] >= 1)

# Mother/Aunt LF
x_m = table(papuan_structure$Family,
      papuan_structure$structure.mother)
sum(x_m[,"112"] >= 1)
nrow(x_m)

# Male Cousins LF
x_cm = table(papuan_structure$Family,
      papuan_structure$structure.cousinM)
sum(x_cm[,"112"] >= 1)
nrow(x_cm)

# Female Cousins LF
x_cf = table(papuan_structure$Family,
      papuan_structure$structure.cousinF)
sum(x_cf[,"112"] >= 1)
nrow(x_cf)

table(papuan_structure$structure.cousinF)

# Omaha language families
table(papuan_structure$Family,
      papuan_structure$structure.omaha)

# Siblings structure
x_sibs = table(papuan_structure$Family,
      papuan_structure$structure.siblings)

table(papuan_structure$Family,
      papuan_structure$structure.siblings) %>% 
  prop.table(., margin = 1) %>% 
  round(., 3)

# n language families per structure
sum(x_sibs[,"1111"] >= 1)
sum(x_sibs[,"1233"] >= 1)

# Grandkin
x_grandM = table(papuan_structure$Family,
      papuan_structure$structure.grandM)

x_grandF = table(papuan_structure$Family,
                 papuan_structure$structure.grandF)

# n language families per structure
sum(x_grandM[,"1111"] >= 1)
sum(x_grandF[,"1111"] >= 1)

## Maps
new_guinea = sf::read_sf("processed_data/base_map.shp")

## Reciprocal grandparents
papuan_structure$structure.grandF_bin = factor(ifelse(papuan_structure$structure.grandF == 1111, 1, 0))
p = ggplot(new_guinea) + 
  geom_sf(fill = "white") + 
  geom_point(data = subset(papuan_structure, !is.na(structure.grandF_bin)), aes(x = Longitude, y = Latitude, fill = structure.grandF_bin), shape = 21) + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  theme_minimal()

## Reciprocal grandparents
papuan_structure$omaha = factor(ifelse(papuan_structure$structure.omaha == 1111, 1, 0))
p = ggplot(new_guinea) + 
  geom_sf(fill = "white") + 
  geom_point(data = subset(papuan_structure, !is.na(omaha)), aes(x = Longitude, y = Latitude, fill = omaha), shape = 21) + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  theme_minimal()

## Save syncretisms 
write.csv(papuan_structure, "processed_data/papuan_syncretisms.csv")
