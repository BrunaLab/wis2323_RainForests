

# install the required packages
# install.packages("tm")		 # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator
# install.packages("RColorBrewer") # color palettes

# load the packages
library("tm")
library("tidytext")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("tidyverse")
library(stopwords)



# Quick Examples

# Read text file

# tutorial  ---------------------------------------------------------------

# For more info on word-clouds see [this tutorial](https://www.geeksforgeeks.org/generating-word-cloud-in-r-programming/)


# load submissions --------------------------------------------------------



words_2021<-read.table("./class_materials/projects_and_code/free-list-word-cloud/words_2021.txt",sep=' ') %>% 
  pivot_longer(where(is.character),
               names_to = "word",) %>% 
  mutate(year=2021) %>% 
  select(year,value) %>% 
  select(-word) %>% 
  rename(word=value) %>% 
  mutate(word=trimws(word)) %>% 
  mutate(words=tolower(words)) 
  

words_2022<-read_csv("./class_materials/projects_and_code/free-list-word-cloud/words_2022.csv") %>% 
  mutate(year=2022) %>% 
  relocate(year,.before=1) %>% 
  mutate(words=tolower(words)) %>% 
  select(-word) %>% 
  rename(word=value) %>% 
  mutate(word=trimws(word)) %>% 
  mutate(words=tolower(words)) 

words_2023<-read_csv("./class_materials/projects_and_code/free-list-word-cloud/words_2023.csv") %>% 
  mutate(year=2023) %>% 
  relocate(year,.before=1) %>% 
  mutate(words=tolower(words)) %>% 
  select(-word) %>% 
  rename(word=value) %>% 
  mutate(word=trimws(word)) %>% 
  mutate(words=tolower(words)) 


words <- bind_rows(words_2021,words_2022,words_2023) %>% 
  mutate(words=gsub(".", "", words, fixed = TRUE)) %>% 
  mutate(words=gsub("south/central america", "central america, south america", words, fixed = TRUE)) %>% 
  mutate(words=gsub("rain/humidity", "rain, humidity", words, fixed = TRUE)) %>% 
  mutate(words=gsub("unique animals/plants", "unique animals, unique plants", words, fixed = TRUE)) %>% 
  mutate(words=gsub("green/fresh", "green, fresh", words, fixed = TRUE)) %>% 
  separate(words,c(letters[seq(1:8)]),sep=",") %>% 
  pivot_longer(where(is.character),
               names_to = "word",) %>% 
  # mutate(word=gsub('\\', "", word, fixed = TRUE)) %>% 
  arrange(word) %>% 
  drop_na() %>% 
  mutate(word=as.factor(word)) %>% 
  mutate(word = case_when(
    word == "amazone" ~ "amazon",
    word == "amazone" ~ "amazon",
    word == "the amazon" ~ "amazon",
    word == "forg" ~ "frogs",
    word == "poison dart frog" ~ "poison dart frogs",
    word == "humid\\" ~ "humid",
    word == "rainy\\" ~ "rain",
    word == "lush\\" ~ "lush",
    word == "lushes" ~ "lush",
    word == "and wet" ~ "wet",
    word == "earth lungs" ~ "lungs of the world",
    word == "and wet" ~ "wet",
    word == "a lot of rain" ~ "rain",
    word == "raindrops" ~ "rain",
    word == "constant rain" ~ "rain",
    word == "high rainfall" ~ "rain",
    word == "rainfall" ~ "rain",
    word == "heavy rains" ~ "rain",
    word == "rainy" ~ "rain",
    word == "animal life" ~ "animals",
    word == "beauty" ~ "beautiful",
    word == "biodiverse" ~ "(bio)diversity",
    word == "forests" ~ "forest",
    word == "diverse" ~ "(bio)diversity",
    word == "diversity" ~ "(bio)diversity",
    word == "biodiversity" ~ "(bio)diversity",
    word == "heat" ~ "hot",
    word == "humidity" ~ "humid",
    word == "lots of noises" ~ "noisy",
    word == "plant life" ~ "plant life",
    word == "monkey" ~ "monkeys",
    word == "monkeys swinging from trees" ~ "monkeys",
    word == "places of high diversity" ~ "(bio)diversity",
    word == "rio- the movie" ~ "the movie 'rio'",
    word == "tall trees and plants" ~ "tall trees",
    word == "tree" ~ "trees",
    word == "treefrogs" ~ "tree frogs",
    word == "vital to ecosystems" ~ "vital",
    word == "waterfall" ~ "waterfalls",
    word == "toucan" ~ "toucans",
    TRUE ~ word)) 
  
wordcloud <- words %>% 
  mutate(word=str_to_title(word)) %>% 
  mutate(word=gsub(" ", "", word, fixed = TRUE)) 
  

# count of unique words
wordcloud %>% summarize(n_distinct(word))

# tibble of unique words
as_tibble(unique(wordcloud$word))

# text<-tolower(wordcloud$word)
text<-wordcloud$word
docs = Corpus(VectorSource(text))   

# Convert the text to lower case
# docs = tm_map(docs, 
#               content_transformer(tolower))
# 

# Remove numbers
# docs = tm_map(docs, removeNumbers)

# Remove white spaces
docs = tm_map(docs, stripWhitespace)



dtm = TermDocumentMatrix(docs, control=list(removePunctuation=T, tolower=F, stopwords=T))

m = as.matrix(dtm)
v = sort(rowSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)
head(d, 10)


document_tm_clean <- removeSparseTerms(dtm, 0.8)
document_tm_clean_mat <- as.matrix(document_tm_clean)



# dev.new(width = 1000, height = 1000, unit = "px")


set.seed(1234)

wc_fig<-wordcloud(words = d$word,
                  freq = d$freq,
                  min.freq = 1,
                  max.words = 290, # Set top n words
                  random.order = FALSE, # Words in decreasing freq
                  random.color=FALSE,
                  # rot.per = 0.35,
                  # scale=c(3,1),  # Set min and max scale
                  rot.per = 0, # % of vertical words
                  fixed.asp = T,
                  # use.r.layout=TRUE, # Use C++ collision detection
                  colors = brewer.pal(8, "BrBG"))


dev.off()