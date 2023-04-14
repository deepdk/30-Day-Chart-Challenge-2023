library(tidyverse)
library(scales)
library(MetBrewer)
library(janitor)
library(geomtextpath)
library(showtext)
library(ggimage)
library(cropcircles)

font_add(family = "Roboto", regular = "D:/HP laptop/Fonts/RobotoCondensed-Regular.ttf")
showtext_auto()

chara <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')
p_stat <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv')

big <- p_stat %>%
filter(uni_name == "The Big Bang Theory")

df1 <- big %>%
select(-char_id, -uni_id, -uni_name, -question, -rank, -rating_sd, -number_ratings)%>%
filter(personality %in% c("awkward","opinionated","emotional","warm","expressive","factual","innocent","jealous","realistic","serious"))%>%
pivot_wider(names_from = personality, values_from = avg_rating,values_fn = list(avg_rating = mean))

df1$image <- c("https://openpsychometrics.org/tests/characters/test-resources/pics/BBT/2.jpg",
               "https://openpsychometrics.org/tests/characters/test-resources/pics/BBT/1.jpg",
               "https://openpsychometrics.org/tests/characters/test-resources/pics/BBT/3.jpg",
               "https://openpsychometrics.org/tests/characters/test-resources/pics/BBT/8.jpg",
               "https://openpsychometrics.org/tests/characters/test-resources/pics/BBT/7.jpg",
               "https://openpsychometrics.org/tests/characters/test-resources/pics/BBT/5.jpg",
               "https://openpsychometrics.org/tests/characters/test-resources/pics/BBT/4.jpg",
               "https://openpsychometrics.org/tests/characters/test-resources/pics/BBT/10.jpg",
               "https://openpsychometrics.org/tests/characters/test-resources/pics/BBT/6.jpg",
               "https://openpsychometrics.org/tests/characters/test-resources/pics/BBT/9.jpg")
               
d1 <- df1 %>%
  mutate(image = circle_crop(image)) %>% 
pivot_longer(cols = c("awkward","opinionated","emotional","warm","expressive","factual","innocent","jealous","realistic","serious"),
            names_to = 'personality',
            values_to = 'value')
            
p1 <- d1 %>%
ggplot(aes(x=as.factor(personality), y=value,  fill=personality)) +      
geom_bar(stat="identity")+
ylim(-50,100) +
geom_image(mapping=aes(y=-50,x=1,image=image), size=0.25)+
scale_fill_manual(values = met.brewer("Johnson",10))+
coord_curvedpolar()+
facet_wrap(~char_name)+
theme_light()+
theme(panel.background = element_rect(fill="black", color="black")) +
theme(plot.background  = element_rect(fill="black", color="black")) +
theme(panel.border     = element_rect(color="black")) +
theme(strip.background = element_rect(fill="black", color="black"))+
theme(strip.background = element_rect(color="black", fill="#00CDCD", size=1, linetype="solid"))+
theme(strip.text.x = element_text(size = 20, color = "black", family = "Roboto"))+
theme(axis.text.x = element_text(size = 15, color = "white", family = "Roboto"))+
  theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(axis.text.y  = element_text(size=15, color = "#FFFFFF", family = 'Roboto'))+
theme(plot.title   = element_text(color="#FFFFFF", size=60, face = "bold", hjust = 0.5, family = 'Roboto'))+
theme(plot.subtitle = element_text(color="#FFFFFF", size=30,hjust = 0.5, family = 'Roboto', face = "bold"))+
theme(legend.position = "none")
p1            
