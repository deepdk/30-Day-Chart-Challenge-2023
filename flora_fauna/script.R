# import libraries
library(tidyverse)
library(janitor)
library(ggsvg)
library(stringr)
library(showtext)
library(jpeg)
library(png)
library(ggpubr)

# set font
font_add(family = "alpaca", regular = "Fonts/Rounded Elegance.ttf")
showtext_auto()

# read data
df <- read_csv("species.csv")

# Add svg icons for spiders
svg_url <- "https://www.svgrepo.com/download/500128/spider.svg"
svg_txt <- paste(readLines(svg_url), collapse = "\n")

# backround image gradient
image = readPNG("background.png")

# plot
p <- df %>% 
  count(species,sort = TRUE) %>% 
  head(10) %>% 
  ggplot(aes(reorder(species, n),n))+
  background_image(image)+
  geom_point()+
  geom_point_svg(aes(species, n),svg = svg_txt, size = 30) +
  geom_segment(aes(x=species, xend=species, y=0, yend=n), color = 'black',size = 1)+
  geom_text(aes(label = str_to_title(species)),position=position_dodge(width=0.9), size = 25,color = "#663333",angle = 90, hjust = -2.5, vjust = 1.5, family = "alpaca")+ 
  scale_y_reverse()+
  annotate("text", x=5, y=75, label= "Spider Species", color = '#663333', size = 80, family = 'alpaca', fontface = "bold")+
  annotate("text", x=5, y=90, label= "There are 4313 genera and 51002 valid spider species to date 2023-04-03\nof which above are the top 10 most found species in the world", color = '#663333', size = 20, family = 'alpaca', fontface = "bold")+
  theme_minimal()+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(plot.caption = element_text(color = "#663333", size = 35, hjust = 0.5, family = "alpaca" ))+
  labs(caption = "Data source : World spider database  Graphic: Deepali Kank")
p
