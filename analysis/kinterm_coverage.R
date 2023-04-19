## This script examines the kinterm coverage of Papuan languages

library(dplyr)

papuan_languages = read.csv('processed_data/papuan_languages.csv')

kinterm_data = read.csv("submodules/kinbank/cldf/forms.csv")

# filter to papuan languages
papuan_data = kinterm_data %>% 
  dplyr::filter(Language_ID %in% papuan_languages$ID)

# what is the kinterm coverage of the data
kinterm_coverage =
  papuan_data %>%
  group_by(Parameter_ID, Language_ID) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(Parameter_ID) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print(kinterm_coverage, n = 20)

##
