library(magick)
# dir_out<-"./IDS2935_class_sessions/12_1_climate_change/stripes/images"
dir_out<-"./class_materials/projects_and_code/climate_stripes_gif/images"


## list file names and read in


imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)



## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "./class_materials/projects_and_code/climate_stripes_gif/ids2935_2022_images.gif")