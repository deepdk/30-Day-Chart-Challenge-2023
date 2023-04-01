library(tidyverse)
library(RColorBrewer)
library(showtext)
library(treemapify)
library(showtext)

font_add(family = "bree", regular = "D:/Fonts/BreeSerif-Regular.ttf")
font_add(family = "arvo",regular = "D:/Fonts/Arvo-Regular.ttf")
showtext_auto()

df <- read_csv("D:/30daychart/part to whole/Protein Costs.csv")

df <- df %>% 
  mutate(Vegetarian = case_when(Vegetarian == 'TRUE' ~ 'Vegetarian',
                         Vegetarian == 'FALSE' ~ 'Non-Vegetarian'))
                         
df$label <- paste(df$Protein_Source, sprintf("$%0.2f", df$Cost_per_20_Grams_of_Protein_USDollars), sep = "\n")

ggplot(df, aes(area = Cost_per_20_Grams_of_Protein_USDollars, fill = Vegetarian, label=label, subgroup=Vegetarian)) +
  geom_treemap(aes(alpha = Cost_per_20_Grams_of_Protein_USDollars)) +
    geom_treemap_subgroup_border(colour="black") +
    geom_treemap_text(family = "arvo",
                      colour = "black",
                      place = "center",
                      grow = T,
                      size = 0.5) +
   geom_treemap_subgroup_text(place = "bottom",
                              family = "bree",
                                 grow = T,
                                 alpha = 0.5,
                                 colour = "#FAFAFA",
                                 size = 0.5)+
    scale_fill_manual(values = c("#FF6347","#00FF7F"))+
    theme_minimal()+
    theme(plot.title = element_text(color="black", size=150, face = "bold",  family = 'bree'))+
    theme(plot.subtitle = element_text(color="black", size=80, face = "bold", family = 'bree'))+
    theme(plot.caption     = element_text(color="black", size=40, face = "bold", family = 'bree'))+
  theme(legend.position = "none")+
  labs(title = "The Cheapest way to Get Your Protein",
       subtitle = "How much does a 20g of protein costs?",
       caption = "DataSource:https://data.world/makeovermonday/2023w8 \nGraphic: Deepali Kank")                         
