library(dplyr)
library(kinbankr)
library(ggplot2)

kinterms = read.csv("submodules/kinbank/cldf/forms.csv")
kinbank_languages = read.csv("submodules/kinbank/cldf/languages.csv")

kinterms = left_join(kinterms, kinbank_languages, by = c("Language_ID" = "ID"))

papuan_languages = read.csv("processed_data/papuan_languages.csv")

# Map
new_guinea = sf::read_sf("processed_data/base_map.shp")

## function
grab_andplot = function(kinterms, legend, title){
  sets = get_structural_vectors(kin_types = kinterms, 
                                         duplicates = "any")
  
  sets = as.data.frame(sets)
  sets$Language_ID = rownames(sets)
  sets$papuan = ifelse(rownames(sets) %in% papuan_languages$ID, "papuan", "non-papuan")
  sets$exchange_terms = ifelse(sets[,1] == TRUE, legend[1], legend[2])
  table(sets$exchange_terms, sets$papuan)
  
  ## add papuan metadata
  sets = left_join(sets, papuan_languages, by = c("Language_ID" = "ID"))
  
  ggplot(new_guinea) + 
    geom_sf(fill = "white") + 
    geom_point(data = sets, aes(x = Longitude, y = Latitude, fill = exchange_terms), shape = 21) +
    ggtitle(title) + 
    theme(legend.title = element_blank())
  
  sets %>% 
    filter(Language_ID %in% papuan_languages$ID) %>% 
    select(Language_ID, papuan, 1)
}

## Husband's sister = brother's wife
hz = grab_andplot(c("fHZ", "mBW"), c("HZ = BW", "HZ != BW"), "Does husband's sister = brother's wife?")
table(hz$mBWfHZ)
## Wifes's brother = sisters's husband
wb = grab_andplot( c("mWB", "mZH"), c("WB = ZH", "WB != ZH"), "Does wife's brother = sister's husband?")
table(wb$mZHmWB)
## 
hzwb = left_join(hz, wb, "Language_ID")

cor(as.numeric(hzwb$mBWfHZ), as.numeric(hzwb$mZHmWB), use = "complete.obs")

hzwb$mBWfHZ = ifelse(hzwb$mBWfHZ == TRUE, "BW=HZ", "Not")
hzwb$mZHmWB = ifelse(hzwb$mZHmWB == TRUE, "ZH=WB", "Not")
table(hzwb$mBWfHZ, hzwb$mZHmWB)



## Wifes's Father is Mother's Brother
grab_andplot( c("mWF", "mMeB"), c("WF = MB", "WF != MB"), "Does wife's father = mother's brother?")

## Wifes's Mother is Fathers's Sister
grab_andplot( c("mWM", "mFeZ"), c("WM = FZ", "WM != FZ"), "Does wife's mother = father's sister?")

## Husbands's Father is Mother's Brother
grab_andplot( c("fHF", "mMeB"), c("WM = FZ", "WM != FZ"), "Does husband's father = mother's brother?")

## Husbands's Mother is Father's Sister
grab_andplot( c("fHM", "mFeZ"), c("HM = FZ", "HM != FZ"), "Does husband's mother = father's sister?")


## Test cases
## Do languages just have a single affine term?
grab_andplot( c("fH", "mW"), c("H = W", "H != W"), "Does husband = wife?")

grab_andplot( c("mWB", "mWF"), c("WB = WF", "WB != WF"), "Does wife's brother = wife's father?")

