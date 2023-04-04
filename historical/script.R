library(tidyverse)
library(janitor)
library(jpeg)
library(png)
library(cropcircles)
library(ggimage)

df <- read_csv("Top 100 women on the web.csv")

df <- df %>% 
  clean_names()
  
# Manually added the images links to the datset  
df <- df %>% 
  mutate(image_croped = circle_crop(image_links))  
  
# Gradient background
image = readPNG("back.png")  

df %>% 
  ggplot(aes(subject_area, most_searched_rank))+
  background_image(image)+
  coord_flip()+
  theme_minimal()+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  
# Added the Title, images in figma  
