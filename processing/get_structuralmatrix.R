# /usr/bin/Rscript

### This script converts the term data into the structural vectors
### For each row in a term subset (which is a language) it will compare
### every term to every other term and convert it to a vector. 
### The comparison metric will be flexible in the function. 

suppressPackageStartupMessages({
  library(stringr)
  library(dplyr)
  library(kinbankr)
})

# parameters
method = "binary"

# languages to remove
to_remove = c("p_asfauslanaust1271") # australian sign language

## siblings
siblings = outer(c("m", "f"), c("eB", "eZ", "yB", "yZ"), paste0)
siblings = c(siblings[1,], siblings[2,])
sibling_vectors = get_structural_vectors(kin_types = siblings, duplicates = "any")
sibling_vectors = sibling_vectors[!rownames(sibling_vectors) %in% to_remove,]

## Parent's and sibs
parents_andsiblings = outer(c("m", "f"), c("M", "F", "MeB", "MyB", "FeB", "FyB", "MeZ", "MyZ", "FeZ", "FyZ"), paste0)
parents_andsiblings = c(parents_andsiblings[1,], parents_andsiblings[2,])
parentsandsibs_vectors = get_structural_vectors(kin_types = parents_andsiblings, duplicates = "any")
parentsandsibs_vectors = parentsandsibs_vectors[!rownames(parentsandsibs_vectors) %in% to_remove,]

# Sibs and cousins
sibs_andcousins = outer(c("m", "f"), c("eB", "eZ", "yB", "yZ",  # siblings
                                       "MBeS", "MByS", "MBeD", "MByD", # mother's brother's children
                                       "MZeS", "MZyS", "MZeD", "MZyD", # mother's sister's children
                                       "FBeS", "FByS", "FBeD", "FByD", # father's brother's children
                                       "FZeS", "FZyS", "FZeD", "FZyD"), paste0) # father's sister's children
sibs_andcousins = c(sibs_andcousins[1,], sibs_andcousins[2,])
sibsandcousins_vectors = get_structural_vectors(kin_types = sibs_andcousins, duplicates = "any")
sibsandcousins_vectors = sibsandcousins_vectors[!rownames(sibsandcousins_vectors) %in% to_remove,]

## save output
write.csv(sibling_vectors, "processed_data/sibling_vectors.csv", row.names = TRUE)
write.csv(parentsandsibs_vectors, "processed_data/parentsandsibs_vectors.csv", row.names = TRUE)
write.csv(sibsandcousins_vectors, "processed_data/sibsandcousins_vectors.csv", row.names = TRUE)

