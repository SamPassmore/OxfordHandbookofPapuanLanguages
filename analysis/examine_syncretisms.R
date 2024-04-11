
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


#' Processing function for calculate structural syncretisms 
#'
#' @param kin_types The kinstypes of interest to be processed.
#' @param papuan_languages A list of languages to analyse separately. 
#' This is always Papuan langauges here, but it needn't be. 
#'
#' @return A list containing the proportions of each structure, the structures 
#' themselves, and chi-squared tests between all pairs of structures. 
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

#' Make a plot of the proportions
#'
#' @param data A dataframe of proportions obtained from the process_function
#' @param title The title of the graph
#' @param labels Labels to include in the graph 
#' @param counts A vector containing the count of langauges within each graph
#'
#' @return a ggplot graph
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

#' A wrapper for a Chi Square test
#'
#'@description
#'This function formats the output of a Chi-Square test for the ggplot in make_proportionplot
#'It is a function internal to process_function.
#' @param summary_table 
#' @param totals 
#'
#' @return
#' @export
#'
#' @examples
chi_test_wrapper = function(summary_table, totals){
  c2 = chisq.test(rbind(summary_table, totals - summary_table), simulate.p.value = TRUE)
  c(c2$statistic, c2$p.value)
}

#### Making the Graphs ####

#### Parent / Nuncle males ####
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
  "Father & Nuncles",
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

#### Aunts ####
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

#### Cousins ####
cousin_types = c("meB", "mFBeS", "mFZeS")
cousinM_structures = process_function(cousin_types, papuan_languages = papuan_languages)
cousinM_structures$structures$Language_ID = rownames(cousinM_structures$structures)

#### Cousin organisation (Males) ####
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

#### Female cousins ####
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


#### Other notable differences ####
## Omaha Cousins ##
omaha_types = c("mMeB", "mMyB", "mMBeS", "mMByS")
omaha_structures = process_function(omaha_types, papuan_languages = papuan_languages)
omaha_structures$structures$Language_ID = rownames(omaha_structures$structures)

plot_omaha = data.frame(omaha_structures$summary)
plot_omaha_long = pivot_longer(plot_omaha, cols = !structure) 

plot_omaha_long = plot_omaha_long %>% 
  filter(value != 0) %>% 
  filter(structure == 1111)

#### Sibling organisation ####
siblings_types = c("meB", "myB", "meZ", "myZ")
sibling_structures = process_function(siblings_types, papuan_languages = papuan_languages)
sibling_structures$structures$Language_ID = rownames(sibling_structures$structures)

plot_sibs = data.frame(sibling_structures$summary)
plot_sibs_long = pivot_longer(plot_sibs, cols = !structure) 

plot_sibs_long = plot_sibs_long %>% 
  filter(value != 0) %>% 
  filter(structure %in% c(1233))

#### Grandparent Reciprocals ####
### Male GP ###
grandM_types = c("mFF", "mMF", "mSS", "mDS")
grandM_structures = process_function(grandM_types, papuan_languages = papuan_languages)
grandM_structures$structures$Language_ID = rownames(grandM_structures$structures)

plot_grandM = data.frame(grandM_structures$summary)
plot_grandM_long = pivot_longer(plot_grandM, cols = !structure) 

plot_grandM_long = plot_grandM_long %>% 
  filter(value != 0) %>% 
  filter(structure %in% c(1111))

### Female GP ###
grandF_types = c("mFM", "mMM", "mSD", "mDD")
grandF_structures = process_function(grandF_types, papuan_languages = papuan_languages)
grandF_structures$structures$Language_ID = rownames(grandF_structures$structures)

plot_grandF = data.frame(grandF_structures$summary)
plot_grandF_long = pivot_longer(plot_grandF, cols = !structure) 

plot_grandF_long = plot_grandF_long %>% 
  filter(value != 0) %>% 
  filter(structure %in% c(1111))

### Join interesting statistics together
interesting_df = list(plot_omaha_long, plot_sibs_long, plot_grandM_long, plot_grandF_long)
names(interesting_df) = c("Omaha", "Siblings", "GrandkinM", "GrandkinF")

interesting_df = bind_rows(interesting_df, .id = "type")
interesting_df$structure.pretty = factor(c("MB = MBS", "MB = MBS", # Omaha labels
                                    "eB ≠ yB ≠ eZ = yZ", "eB ≠ yB ≠ eZ = yZ", # Sibling org. 1
                                    "FF = MF = SS = DS", "FF = MF = SS = DS", # Grandkin M
                                    "MM = FM = SD = DD", "MM = FM = SD = DD" # Grandkin F
                                    ), 
                                    levels = c("eB ≠ yB ≠ eZ = yZ",
                                               "FF = MF = SS = DS", "MM = FM = SD = DD",
                                               "MB = MBS"))

## Calculate counts ## 
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
  annotate("text", x = 1.0, y=0.53, label = "Siblings", hjust = 0.5, size = 7) +
  annotate("text", x = 2.5, y=0.53, label = "Grandkin", hjust = 0.5, size = 7) +
  annotate("text", x = 4.0, y=0.53, label = "Matrilineal\nSkewing", hjust = 0.5, size = 7) +
  annotate("label", x = 1.4, y=0.42, label = sib_counts, hjust = 1, label.r = unit(0, "pt")) +
  annotate("label", x = 1.6, y=0.42, label = gm_counts, hjust = 0, label.r = unit(0, "pt")) +
  annotate("label", x = 3.4, y=0.42, label = gf_counts, hjust = 1, label.r = unit(0, "pt")) +
  annotate("label", x = 4.4, y=0.42, label = om_counts, hjust = 1, label.r = unit(0, "pt")) +
  geom_vline(xintercept = 1.5) + 
  geom_vline(xintercept = 3.5) + 
  theme(legend.title=element_blank()) +
  ggpubr::geom_bracket(
    xmin = .75, xmax = 1.25,
    y.position = 0.328, label = sibling_structures$chi2[3,"1233"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 1.75, xmax = 2.25,
    y.position = 0.411, label = grandM_structures$chi2[3,"1111"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 2.75, xmax = 3.25,
    y.position = 0.352, label = grandF_structures$chi2[3,"1111"],
    tip.length = 0.04
  ) +
  ggpubr::geom_bracket(
    xmin = 3.75, xmax = 4.25,
    y.position = 0.212, label = omaha_structures$chi2[3,"1111"],
    tip.length = 0.04
  )

#### Build Figure 3 ####
(p_father + p_mother) / (p_cousinM + p_cousinF) / p_interesting +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
grid::grid.draw(grid::textGrob("Proportion", x = 0.007, rot = 90))
ggsave("proportions_plot.png", height = 300, units = "mm")
