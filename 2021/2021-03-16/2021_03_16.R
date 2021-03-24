# Title: TidyTuesday, 2021-03-16
# Description: Video Games and Sliced
# Author: Tuo Wang
# Date: 2021-03-23

library(tidyverse)
library(here)         # constructs
library(skimr)        # skim the data
library(lubridate)    # deal with date object
library(showtext)     # download fonts
library(ggrepel)      # automatically position non-overlapping text labels 

# font_add_google("Lato", "Lato")
# font_add_google("Merriweather", "Merriweather")
# font_add_google("Gochi Hand", "gochi")
# font_add_google("Chango", "chango")
# font_add_google("Lobster", "lobster")
# font_add_google("Bangers", "banger")
# font_add_google("Roboto Slab", "roboto")
showtext::font_add_google("Luckiest Guy", "luckiest")
showtext::font_add_google("Poppins", "poppins")
showtext::showtext_auto()

# Read Data
tuesdata <- tidytuesdayR::tt_load('2021-03-16')
game <- tuesdata$games

# skimr::skim(game)

# game %>%
#   group_by(year, month) %>%
#   summarise(n = n()) %>%
#   arrange(desc(year), match(month,month.name)) %>%
#   head(20)
# 
# game %>%
#   group_by(year) %>%
#   summarise(n = n_distinct(month)) %>%
#   arrange(desc(year)) 

# game %>% 
#   filter(gamename== "PLAYERUNKNOWN'S BATTLEGROUNDS") %>%
#   View()

# Construct new variables for better visualization
df <- game %>% 
  group_by(year, month) %>%
  summarise(avg_all_game = mean(avg),
            peak_all_game = mean(peak),
            .groups='drop') %>%
  mutate(date = ymd(paste(year, month, '15', sep='-')),
         month_num = match(month, month.name),
         season = case_when(
           month_num %in% c(12,1,2) ~ 'Winter',
           month_num %in% c(3,4,5) ~ 'Spring',
           month_num %in% c(6,7,8) ~ 'Summer',
           month_num %in% c(9,10,11) ~ 'Autumn'),
         pandemic = case_when(
           date > ymd("2020-01-01") ~ 'yes',
           TRUE ~ 'no'),
         highlight = case_when(
           date == ymd("2020-01-15") ~ 1,
           date == ymd("2020-04-15") ~ 1,
           TRUE ~ 0),
         label = case_when(
           date == ymd("2020-01-15") ~ str_wrap("Covid-19"),
           date == ymd("2020-04-15") ~ str_wrap("Stay at home"),
           TRUE ~ NA_character_)) %>%
  arrange(date) 

df %>%
  ggplot() +
  geom_line(aes(x=date, y=avg_all_game),size=1.5, color="#575757") + 
  geom_line(aes(x=date, y=peak_all_game),size=1.5,color="#82786f") +
  annotate("text", x = ymd("2012-07-15"), y = 2500, fontface="bold",
           label = "Average",family="poppins",color="#575757") +
  annotate("text", x = ymd("2012-07-15"), y = 4500, fontface="bold",
           label = "Peak",family="poppins",color="#82786f") +
  geom_line(data=filter(df, pandemic=='yes'),aes(x=date, y=avg_all_game),
            size=1.5, color="#fa5555") +
  geom_line(data=filter(df, pandemic=='yes'),aes(x=date, y=peak_all_game),
            size=1.5, color="#fa5555") +
  geom_point(data = filter(df, highlight==1),aes(x=date, y=avg_all_game),
             size=3, shape=21,fill="#fa5555",color="white") +
  geom_point(data = filter(df, highlight==1),aes(x=date, y=peak_all_game),
             size=3, shape=21,fill="#fa5555",color="white") +
  ggrepel::geom_label_repel(data = filter(df, highlight==1),hjust=1.3,vjust=-2,
             aes(x=date, y=peak_all_game,label=label),family = "poppins") +
  labs(title="Steam Gaming Statistics from 2012 to 2021",
       subtitle = "Highest and average number of players at the same time",
       x=NULL, y=NULL,caption = "Data: TidyTuesday") +
  scale_x_date(date_breaks = "2 year",date_labels = "%Y") +
  theme(
    text = element_text(family = "luckiest", size = 8, color = "black"),
    plot.title = element_text(family = "luckiest", size = 20,
                                  face = "bold", color="#2a475e"),
    plot.subtitle = element_text(family = "poppins", size = 15, 
                                 face = "bold", color="#1b2838"),
    plot.caption = element_text(size = rel(1), hjust = 0),
    axis.text = element_text(size = rel(1.25), color = "black"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    rect = element_blank(),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    #panel.grid.major.x = element_line(linetype="dashed"),
    #panel.grid.major.y = element_blank(),
    panel.grid.major.y = element_line(linetype="dashed"),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = '#fbf9f4', color = '#fbf9f4')
  )


ggsave(
  filename = here::here("2021","2021-03-16","2021-03-16.png"),
  width = 8,
  height = 4.5,
  device = "png"
)
