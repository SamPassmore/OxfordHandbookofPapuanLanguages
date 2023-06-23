library(ggplot2)
library(dplyr)

iter = 20
runs = data.frame()
for(i in 1:iter){
  print(i)
  system("RScript ./analysis/umap_projections.R")
  system("RScript ./analysis/functional_richness.R")
  
  umap = read.csv("results/umap_functionalrichness.csv")
  mds = read.csv("results/mds_functionalrichness.csv")
  fric = list(umap = umap, mds = mds)
  fric = bind_rows(fric, .id = "type")
  
  fric$iter = i
  
  runs = rbind(runs, fric)
}

runs_ss = runs[runs$site != "Global",]

ggplot(runs_ss, aes(y = FRic, x = type)) + 
  geom_boxplot() +
  geom_jitter() + 
  facet_wrap(~id + site)

