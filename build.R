## This script will build all analyses used in section 3.1 of Chapter 40 in the 
## Oxford Handbook of Papuan Languages 

## This script requires the Kinbank submodule is activated before running. 
## To initialize the submodule you can use the commented command below. 
## This only needs to be performed once. 

# system("git submodule init; git submodule update")

## The scripts below are listed in running order. I have indicated where 
## particular figures or tables are created. 

# 1. Identify all Papuan languages in Kinbank & calculate some descriptive statistics
system("Rscript processing/get_papuanlanguages.R")

# 2. Get structural vectors of the Papuan and other languages
system("Rscript processing/get_structuralmatrix.R")
system("Rscript processing/some_fixes.R")

# 3. Make maps 
system("Rscript processing/base_map.R")
system("Rscript processing/make_map.R") # Creates Figure 2

# 4. Identify syncritisms where Papuan languages stand out
system("Rscript analysis/examine_syncretisms.R") # Creates Figure 3

# 5. How many unique Papuan structures are there?
system("Rscript analysis/unique_papuan_structures.R") # Creates data for Table 2

# 6. Functional Richness analysis projections & Average distance between structures
system("Rscript analysis/functional_richness.R") # Creates Figure 4
