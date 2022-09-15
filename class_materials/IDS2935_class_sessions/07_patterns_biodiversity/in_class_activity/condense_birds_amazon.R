library(tidyverse)
library(readxl)
# data from: https://datadryad.org/stash/dataset/doi:10.5061%2Fdryad.j14g206
# Borges, Sergio H.; Baccaro, Fabricio; Moreira, Marcelo; Choueri, Erik, L. (2019),
# Data from: Bird assemblages on Amazonian river islands: patterns of species diversity 
# and composition, Dryad, Dataset, https://doi.org/10.5061/dryad.j14g206
birds_am<-read_xlsx("./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/raw_data/Supplementary_material.xlsx", sheet="Capture_data",skip=1) %>% 
  select(-Seq_alfa,-Seq_tax) %>%
  rename(species=Species) %>% 
  mutate(n = rowSums(across(where(is.numeric)))) %>% 
  select(species,n) %>% 
  arrange(desc(n)) %>% 
  separate(species, remove=FALSE,c("a","b")) %>% 
  mutate(code=paste(str_extract(a, "^.{2}"),str_extract(b, "^.{2}"), sep="")) %>% 
  mutate(code=toupper(code)) %>% 
  select(code,species,n)
birds_am

  
  




head(birds_am,30)
str(birds_am)
write_csv(birds_am,"./class_materials/IDS2935_class_sessions/07_patterns_biodiversity/in_class_activity/raw_data/birds_am.csv")

birds_am<-birds_am %>% 
  mutate(csum = cumsum(n)) %>% 
  mutate(cperc = csum/sum(n))
birds_am
birds_am_last_half<-birds_am %>% slice_tail(n=(nrow(birds_am)/2))
# p<-ggplot(birds_am, aes(x=reorder(species, desc(1-cperc)), y=cperc)) + 
p<-ggplot(birds_am, aes(x=reorder(species, desc(n)), y=n)) +
  geom_bar(stat = "identity")+
  xlab("Species")+
  # scale_y_continuous(name = "Number of Trees", limits = c(0, 30000)) +
  scale_y_continuous(breaks=seq(0, 200, 20))+
  theme(axis.text.x = element_text(angle = 45,hjust=1))

p

hist(birds_am$n)
sum(birds_am$n)