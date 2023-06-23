## This is the build script

# 1. Identify all Papuan languages & Get descriptives
system("Rscript processing/get_papuanlanguages.R")

# 2. Get structural vectors of the Papuan and other languages
system("Rscript processing/get_structuralmatrix.R")
system("Rscript processing/some_fixes.R")

# 3. Make maps 
system("Rscript processing/base_map.R")
system("Rscript processing/make_map.R")
system("Rscript processing/visualise_syncretisms.R")

# 4. Identify syncritisms where Papuan languages stand out
system("Rscript analysis/examine_syncretisms.R")

# 5. How many unique Papuan structures are there?
system("Rscript analysis/unique_papuan_structures.R")

# 6. Functional Richness analysis projections & Average distance between structures
system("Rscript analysis/functional_richness.R")
system("Rscript analysis/FR_samplingAustronesian.R") # Random sampling of Austronesian languages

# 7. Average distance between languages & language families 
system("Rscript analysis/average_distance.R")
