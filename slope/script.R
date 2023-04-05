library(tidyverse)
library(janitor)
library(CGPfunctions)


# https://www.kaggle.com/datasets/iamsouravbanerjee/cause-of-deaths-around-the-world
df <- read_csv("cause_of_deaths.csv")

df <- df %>% 
  clean_names()
  
year_2 <- c(1990,2019)

df <- df %>% 
  filter(year %in% year_2) %>% 
  pivot_longer(cols = c(4:34),
               names_to = 'cause',
               values_to = 'value') %>% 
  filter(cause == "cardiovascular_diseases") 
  
country_1 <- c("Canada", "France", "Germany", "Italy", "Japan", "United Kingdom","United States")

df <- df %>% 
  filter(country_territory %in% country_1) %>% 
  mutate(year = as.character(year))
  
newggslopegraph(df, 
                year, 
                value, 
                country_territory,
                DataLabelPadding = 0.2,
                DataLabelLineSize = 0.5,
                DataLabelFillColor = "#E0EEE0",
                LineThickness = 1.5,
                XTextSize = 10,    # Size of the times
                YTextSize = 10,
                DataTextSize = 8,
                LineColor = "#B22222")
                
#Added the title, caption using figma                
