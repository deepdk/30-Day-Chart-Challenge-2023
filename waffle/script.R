library(tidyverse)
library(showtext)
library(showtext)
library(janitor)
library(waffle)

df <- read_csv("happy.csv")
con <- read_csv("continents2.csv")

font_add(family = "Roboto", regular = "/kaggle/input/roboto/RobotoCondensed-Regular.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#030303"
  color.text ="#F2F2F2"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
theme(strip.background = element_rect(fill=color.background, color=color.background)) +
# Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.minor.x = element_blank()) +
    theme(axis.ticks       = element_blank()) +
# Format the legend
    theme(legend.position = "none") +
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.text = element_text(size = 15, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "Roboto",
                                    color = "#030303",
                                    size = 15, face = "bold"))+
 # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_blank()) +
    theme(axis.text.y      = element_blank()) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
    }                                   
                                    

df <- df %>% 
  clean_names()

con <- con %>% 
  clean_names()
  
df1 <- df %>%
mutate(country_name = case_when(country_name == "Czechia" ~ "Czech Republic",
                                country_name == "Taiwan province of china" ~ "Taiwan",
                                country_name == "Kosovo" ~ "Albania",
                                country_name == "Bosnia and Herzegovina" ~ "Bosnia And Herzegovina",
                                country_name == "Hong Kong S.A.R. of China" ~ "Hong Kong",
                                country_name == "Congo (Brazzaville)" ~ "Congo",
                                country_name == "North Macedonia" ~ "Macedonia",
                                country_name == "Ivory Coast" ~ "Côte D\'Ivoire",
                                country_name == "State of Palestine" ~ "Palestine, State of",
                                country_name == "Turkiye" ~ "Turkey",
                                country_name == "Congo (Kinshasa)"~ "Congo (Democratic Republic Of The)",TRUE~country_name))
df1$country_name

df2 <- con %>% 
  select(name, sub_region) %>% 
  rename("country_name" = "name")
head(df2)

final <- left_join(df1,df2,by="country_name")
head(final)

dimensions <- c('ladder_score','logged_gdp_per_capita','social_support','healthy_life_expectancy','freedom_to_make_life_choices','generosity','perceptions_of_corruption')

# map country to regions
country_region_dict = final %>% select(country = country_name, region = sub_region) %>% unique()

df_2023_long <- final %>% 
    select(country = country_name, all_of(dimensions)) %>%
    mutate(absence_of_corruption = 1- perceptions_of_corruption) %>%
    pivot_longer(cols = c(all_of(dimensions)), names_to = 'dimension', values_to = 'score')

df_2023_tranformed <- df_2023_long %>%
    group_by(dimension) %>%
    mutate(min_value = min(score),
             max_value = max(score)) %>%
    mutate(score_pct = (score-min_value)/(max_value-min_value)) %>%
    ungroup()
    
df_waffle <- df_2023_tranformed %>%
    filter(dimension == 'ladder_score') %>%
    left_join(country_region_dict, by = 'country') %>%
    mutate(score_bin = cut(score, seq(2,8, 1), right = FALSE)) %>%
    group_by(region) %>%
    mutate(region_avg = mean(score)) %>%
    ungroup() %>%
    mutate(region = reorder(region, region_avg)) %>%
    count(region, score_bin) %>%
    arrange(score_bin, n)

score_levels = levels(df_waffle$score_bin)   

pal <- colorRampPalette(c("#C2C2C2", "white", "#FFFF00"))

df_waffle %>%
filter(!is.na(region))%>%
ggplot(aes(fill = score_bin, values = n)) +
  geom_waffle(color = "#424242", n_rows = 6, flip = TRUE) +
  facet_wrap(~region, nrow =3,strip.position = "bottom", labeller = label_wrap_gen()) +
  scale_fill_manual(
    name = "",
    values = pal(6),
    labels = c('','','','','',''))+
  coord_equal() +
my_theme() +
theme(legend.position = c(0.85, 1))+
theme(legend.box = 'horizontal')+
theme(legend.key.size = unit(1,'cm'),
        legend.title = element_text(size = 18, hjust = 1),
        legend.text = element_text(size = 16))+
guides(fill = guide_legend(reverse = FALSE, nrow = 1, byrow = TRUE))

# Added the title,subtitle and smiley image using figma
    
