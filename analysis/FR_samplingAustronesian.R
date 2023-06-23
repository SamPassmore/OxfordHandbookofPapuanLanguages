# Calculate FR for subsets of each languages family

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(fundiversity)
  library(ggforce)
})

# Language metadata
kinbank_languages = read.csv('submodules/kinbank/cldf/languages.csv')
papuan_languages = read.csv('processed_data/papuan_languages.csv')

kinbank_languages$Family[kinbank_languages$ID %in% papuan_languages$ID] = "Papuan"

structure_files = list.files('processed_data/', "vectors.csv", full.names = TRUE)
structural_vectors = sapply(structure_files, read.csv, row.names = 1)


# Structural vectors
structures = read.csv('processed_data/sibling_vectors.csv', row.names = 1)


get_fr = function(structures){
  ## Calculate distance matrix
  dist_mat = cluster::daisy(structures, metric = "manhattan", warnBin = FALSE)
  
  # Get MDS dimensions
  kinbank_mds = dist_mat %>% 
    cmdscale(eig = TRUE, k = 2)
  
  # subset to languages used
  kb_l = subset(kinbank_languages, kinbank_languages$ID %in% rownames(structures))
  
  kb_l$family_analysed = ifelse(kb_l$Family %in% c("Indo-European", "Austronesian", "Pama-Nyungan", "Papuan"),
                                kb_l$Family, NA)
  
  table(kb_l$family_analysed)
  
  iter = 1000
  fr_list = list()
  for(i in 1:iter){
    # subset to 100 languages per family 
    language_samples = kb_l %>% 
      group_by(family_analysed) %>% 
      slice_sample(n = 102) %>% 
      pull(ID)
    
    kb_l$family_analysed_100 = ifelse(kb_l$ID %in% language_samples, kb_l$family_analysed, NA)
    
    # Make grouping matrix for language families
    grouping = table(kb_l$family_analysed_100, kb_l$ID)
    
    # Add a grouping for Global diversity
    grouping = rbind(grouping, Global = 1)
    
    ## Return
    fr_list[[i]] = fundiversity::fd_fric(traits = kinbank_mds$points, 
                                         sp_com = grouping, 
                                         stand = TRUE)
    
  }
  
  fr = bind_rows(fr_list)
  
  p = ggplot(data = fr, aes(x = site, y = FRic)) + 
    geom_boxplot() + 
    geom_jitter(alpha = 0.5, width = 0.25)
  p
}

xx = lapply(structural_vectors, get_fr)
