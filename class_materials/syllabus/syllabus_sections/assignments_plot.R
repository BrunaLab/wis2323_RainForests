# library
library(ggplot2)

# create a dataset
semester <- c("Assignments")
tasks <- c("In-class\nActivities" , "Movie\nReview" ,
           "Analytic\nEssay","Reflective\nEssay","Final\nProject")
points <- c(400,100,150,150,200)
data <- data.frame(semester,tasks,points)
data<-as_tibble(data) %>% 
  mutate(tasks=as.factor(tasks),
         semester=as.factor(semester)
         ) %>% 
  arrange(points) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(tasks=factor(tasks, levels=tasks)) %>% 
  mutate(perc=round(points/sum(points)*100),2) %>% 
  mutate(barlabel=paste(tasks,points,sep="\n")) %>% 
  mutate(barlabel=paste(barlabel,"pts",sep=" ")) %>% 
  mutate(barlabel=paste(barlabel,"\n(", perc,"%)", sep="")) 

# # Stacked + percent
# ggplot(data, aes(fill=tasks, y=points, x=semester)) + 
#   geom_bar(position="fill", stat="identity")

library(RColorBrewer)
library("viridis")   
# Stacked
hw_plot<-ggplot(data, 
       aes(fill=tasks, label = barlabel, y=points, x=semester)) + 
  scale_colour_brewer(palette = "YlOrRd", direction = - 1) + 
  scale_fill_brewer(palette = "Paired", direction=-1)+
  # scale_fill_viridis(discrete = T, option="D") +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 6, position = position_stack(vjust = 0.5),fontface="bold", color="white")+
  coord_flip()+
  # geom_col(width = 0.5)+
  # annotate("text", 
  #          x=1.4, 
  #          y=3, 
  #          label= paste("Total course points:",sum(data$points),sep = " "),
  #          fontface="bold",
  #          size=8,
  #          color="white",
  #          hjust="left") + 
  # theme_classic()+
  theme_void()+
  # theme(plot.margin = unit(c(0,4,0,0), "pt")) +
  theme(legend.position="none") 

ggsave("./class_materials/syllabus/course_schedule/icons/hw.png", width = 30, height = 5, units="cm")




