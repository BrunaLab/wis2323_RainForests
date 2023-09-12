library(tidyverse)
library(readxl)
library(janitor)
library(openxlsx)




# 2022 --------------------------------------------------------------------

    file_path <- paste("~/Dropbox (UFL)/Teaching/IDS 2935 - Future of Rain Forests/IDS2935_RainForests/class_materials/projects_and_code/trope_analysis/submissions_2022","/",sep="")
    # file_path %>% list.files()

    file_names <- file_path %>%
      list.files() %>%
      .[str_detect(., ".xlsx")]
    # 
    # file_names <- paste(file_path,file_names,sep="")
    file_names
    
    file.list <- list.files(path = file_path,    
                            pattern = "*.xlsx",
                            full.names = TRUE) 
    
    tropes22<-file.list %>%
      set_names(.) %>%
      map_df(~mutate_all(read_excel(.x), as.character), .id = 'grp') %>%
      mutate(grp = str_remove(basename(grp), ".xlsx")) %>%
      separate(grp, c('student', 'file'), sep = '_', extra = 'merge') %>% 
      select(-student) %>% 
      remove_empty(c("rows", "cols")) %>% 
      mutate(Time_Stamp=gsub("@","",Time_Stamp)) %>% 
      mutate(Time_Stamp=gsub(";","",Time_Stamp)) %>% 
      mutate(Time_Stamp=gsub(" ","",Time_Stamp)) %>% 
      mutate(Time_Stamp=gsub("m",":",Time_Stamp)) %>% 
      mutate(Time_Stamp=gsub("s",":",Time_Stamp)) %>% 
      mutate(Time_Stamp=gsub("!","1",Time_Stamp)) %>% 
      # mutate(Time_Stamp=gsub("1899-12-31","",Time_Stamp)) %>% 
      drop_na("Movie","Time_Stamp","Trope_ID_No.","Brief_Description") %>% 
      mutate_all(as.character()) %>% 
      mutate(Time_Stamp2 = if_else(
        str_detect(Time_Stamp,":"),Time_Stamp,NA)) %>% 
      mutate(Time_Stamp = if_else(
        is.na(Time_Stamp2)==FALSE,NA,Time_Stamp)) %>% 
      mutate(Time_Stamp=trimws(Time_Stamp)) %>% 
      mutate(Time_Stamp=convertToDateTime(Time_Stamp)) %>% 
      mutate(Time_Stamp = if_else(
        is.na(Time_Stamp)==TRUE,Time_Stamp2,NA)) %>% 
      mutate(Time_Stamp=gsub("1899-12-31","",Time_Stamp)) %>% 
      select(-Time_Stamp2)
    
      
    names(tropes22)<-tolower(names(tropes22))
    
    
    
    
    
    
    
    # l <- list.files(path = file_path,  
    #                 pattern = "*.xlsx", full.names = TRUE) %>% 
    #   # lapply(read_xlsx, col_types="text",trim_ws=TRUE,col_names = c("event_no","time_stamp","trope","notes","x","x2"),skip=1)
    #   lapply(read_xlsx, col_types="text",trim_ws=TRUE)
    # 
    #  tropes22  <-  purrr::map_df(l, dplyr::bind_rows,.id = "id") %>% 
    #   drop_na(Movie)
    # 
    #  names(tropes22)<-tolower(names(tropes22))
    #  
     
     



tropes22<-tropes22 %>% 
  mutate(movie=tolower(movie)) %>% 
  mutate(movie = case_when(
  movie == "the african queen" ~ "african queen",
  movie == "aq" ~ "african queen",
  movie == "the african queen (1951)" ~ "african queen",
  # movie == "a.q." ~ "african queen",
  movie == "a.q" ~ "african queen",
  movie == "indigenous" ~ "indigenous: chupacabra",
  movie == "jumanji:welcome to the jungle" ~ "jumanji",
  movie == "jumanji: welcome to the jungle" ~ "jumanji",
  movie == "anaconda (2)" ~ "anaconda",
  movie == "the jungle book" ~ "jungle book",
  movie == "african queen (1)" ~ "african queen",
  movie == "indigenous - chupacabra" ~ "indigenous: chupacabra",
  movie == "indigenous -\r\nchupacabra" ~ "indigenous: chupacabra",
  movie == "welcome to the jungle (2013)" ~ "welcome to the jungle",
  movie == "anoconda" ~ "anaconda",
  movie == "anocanda" ~ "anaconda",
  movie == "anaconda (1997):" ~ "anaconda",
  movie == "anaconda (1997)" ~ "anaconda",
  movie == "1" ~ "anaconda",
  movie == "#1" ~ "african queen",
  movie == "1.0" ~ "african queen",
  movie == "4" ~ "african queen",
  movie == "#1" ~ "anaconda",
  movie == "#2" ~ "anaconda",
  movie == "2.0" ~ "anaconda",
  movie == "2" ~ "anaconda",
  movie == "3.0" ~ "jumanji",
  movie == "3" ~ "jumanji",
  movie == "b.m" ~ "blood monkey",
  movie == "34" ~ "anaconda",
  movie == "a" ~ "anaconda",
  movie == "ac" ~ "anaconda",
  movie == "tz" ~ "tarzan",
  movie == "#3" ~ "apocalypto",
  movie == "the african queen " ~ "african queen",
  movie == "43" ~ "turistas",
  movie == "ferngully: the last rainforest" ~ "ferngully",
  movie == "26" ~ "dna",
  TRUE ~ movie
)) %>% 
  arrange(movie) %>% 
  remove_empty(c("rows", "cols")) 
  
tropes22$movie<-gsub("the african queen","african queen",tropes22$movie)
tropes22$movie<-gsub("\\(1951)","",tropes22$movie) 
tropes22$movie<-gsub("\\(1997)\\:","",tropes22$movie) 
tropes22$movie<-gsub("[[:space:]]*$","",tropes22$movie)
tropes22$movie<-gsub("pirahnaconda","piranhaconda",tropes22$movie)
tropes22$movie<-gsub("^ *|(?<= ) | *$", "", tropes22$movie, perl=T)

tropes22$movie=str_trim(tropes22$movie)
tropes22<-tropes22 %>% mutate(movie = str_replace(movie, " ", "_"))


# tropes22<-tropes22 %>% str_squish(tropes22)


tropes22

sort(unique(tropes22$movie))


tropes<-readxl::read_excel("./tropes.xlsx") %>%
  select(-Category) %>% 
  mutate(ID = row_number(), .before=Trope)


code <- strsplit(tropes$Trope, " ")
code<-sapply(code, function(x){
  toupper(paste(substring(x, 1, 1), collapse = ""))
})
code<-as.data.frame(code)
code <- code %>% 
  mutate(ID = row_number())
str(code)
str(tropes)
tropes <- left_join(code,tropes) %>% 
  select(ID, Trope, Code=code,Description,Link)
names(tropes)<-tolower(names(tropes))

tropes<-tropes %>% 
  select(id,code,trope) %>% 
  mutate_all(tolower) %>% 
  rename(trope_id=id)
rm(code)

tropes22<-tropes22 %>% 
  rename(trope_id=`trope_id_no.`) %>% 
  mutate_all(tolower) %>% 
  left_join(tropes) %>% 
  relocate(file,.after=last_col()) %>% 
  relocate(c(code,trope),.after=trope_id)

# 2021 --------------------------------------------------------------------



movies<-c("aguirre",
          "ferngully",
          # "apocalypto",
          "jungle_book",
          "african_queen",
          "avatar",
          "blood_monkey",
          "elves",
          "fire_on_the_amazon",
          "green_inferno",
          "indigenous",
          "mission",
          "mosquito",
          "mysterious_island",
          "perfect_getaway",
          "predator",
          "rio2",
          "romancing_stone",
          "rundown")

form_binder <- function(movies) {
  tropes <- vector("list", length(movies))
  
  for(i in seq_along(movies)) {                       
  # i<-1
    # i<-8
  file_path <- paste("~/Dropbox (UFL)/Teaching/IDS 2935 - Future of Rain Forests/IDS2935_RainForests/class_materials/projects_and_code/trope_analysis/submissions/",
                     movies[i],"/",sep="")
  # file_path %>% list.files()
  
  
  file_names <- file_path %>%
    list.files() %>%
    .[str_detect(., ".xlsx")]
  
  file_names <- paste(file_path,file_names,sep="")
  file_names
  
  
  l <- list.files(path = file_path,  
                  pattern = "*.xlsx", full.names = TRUE) %>% 
    # lapply(read_xlsx, col_types="text",trim_ws=TRUE,col_names = c("event_no","time_stamp","trope","notes","x","x2"),skip=1)
    lapply(read_xlsx, col_types="text",trim_ws=TRUE)
  
  df <-  purrr::map_df(l, dplyr::bind_rows,.id = "id") %>% 
    drop_na()
  
  df$film<-movies[i]
  tropes[[i]]<-df
  
  }
  
return(tropes)

  
}

tropes<-form_binder(movies)

tropes<-as_tibble(do.call(rbind, tropes)) %>% 
  relocate("film",.after=1) %>% 
  rename(event=Event_No,
         time=Time_Stamp,
         trope=Trope_Abbrev,
         notes=Brief_Description_or_Notes) %>% 
  mutate(trope=tolower(trope)) %>% 
  mutate(trope=trimws(trope))

unique(tropes$trope)






   df_ag<-form_binder("aguirre")

df_fern<-form_binder("ferngully")

# df_apoc<-form_binder("apocalypto") pdf only
df_book<-form_binder("jungle_book")

df_aq<-form_binder("african_queen")

df_av<-form_binder("avatar")

df_bld<-form_binder("blood_monkey")

df_elves<-form_binder("elves")

df_fire<-form_binder("fire_on_the_amazon")

df_inferno<-form_binder("green_inferno")

df_indig<-form_binder("indigenous")

df_mission<-form_binder("mission")

open <-form_binder("mosquito")

df_island<-form_binder("mysterious_island")

df_getaway<-form_binder("perfect_getaway")

df_pred<-form_binder("predator")

df_rio2<-form_binder("rio2")

df_stone<-form_binder("romancing_stone")

df_run<-form_binder("rundown")
