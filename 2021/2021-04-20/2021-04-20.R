# Title: TidyTuesday, 2021-04-20
# Author: Tuo Wang
# Date: 2021-04-20

library(tidyverse)
library(lubridate)
library(showtext)
library(gggibbous)
library(ggtext)

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto slab")
font_add_google("Bebas Neue", "bebas neue")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2021-04-20')
netflix <- tuesdata$netflix

netflix_comp <- netflix %>%
  mutate(
    type = factor(type),
    date_added_num = mdy(date_added),
    added_year = year(date_added_num)) %>%
  drop_na(added_year)

year_stat <- netflix_comp %>%
  group_by(added_year) %>%
  summarise(num_year = n()) %>%
  ungroup() %>%
  mutate(cum_year = cumsum(num_year))

all_stat <- netflix_comp %>%
  group_by(added_year, type, .drop = FALSE) %>%
  summarise(num_year_type=n()) %>%
  ungroup() %>%
  group_by(type) %>%
  mutate(cum_type = cumsum(num_year_type)) %>%
  ungroup() %>%
  right_join(year_stat, by="added_year") %>%
  mutate(
    prop_type_year = num_year_type/num_year,
    prop_type = cum_type/cum_year,
    right = (type=="Movie"))

# text_df <- all_stat %>% 
#   filter(type=="Movie") %>%
#   mutate(label = paste0("<span style='color:white'>",added_year,"</span><br>",
#     "*<span style='color:#a0c4ff'>",sprintf("%1.0f%%",100*(1-prop_type)),"</span>*", 
#     "<span style='color:white'>:</span>", 
#     "*<span style='color:#ffd6a5'>",sprintf("%1.0f%%",100*(prop_type)),"</span>*"
#   ))

text_df <- all_stat %>% 
  filter(type=="Movie") %>%
  mutate(label = paste0("**<span style='color:white'>",added_year,"</span><br>**"))

title_df1 <- data.frame(
  x = 2010.5,
  y = 7.5,
  label="<b style='font-size:45pt;'><span style='color:#E50914'>Netflix</span></b>"
)

title_df2 <- data.frame(
  x = 2010.5,
  y = 6.5,
  label="<b style='font-size:14pt;'><span style='color:white'>Logarithm of the cumulative sum of titles<br>
  <b style='font-size:14pt;'><span style='color:white'>Comparison between <span style='color:#fcbf49'>**Movies**</span> and 
    <span style='color:#e63946'>**TV Shows**</span></span></b>"
)

col_pal <- c("#e63946","#f6bd60")
all_stat %>%
  ggplot() +
  geom_line(aes(x=added_year,y=log(cum_year)),
            size=2.5, color="grey70")+
  geom_moon(
    aes(x=added_year,y=log(cum_year), ratio=prop_type, 
        fill=right, right=right),size=16,
    show.legend = FALSE, color=NA
    ) +
  geom_richtext(
    data = text_df,
    aes(x=added_year,y=log(cum_year)-1.0,label=label),
    fill = NA, label.color = NA, family='roboto')+
  geom_richtext(
    data = title_df1,
    aes(x=x,y=y,label=label),
    fill = NA, label.color = NA, family='bebas neue') +
  geom_richtext(
    data = title_df2,
    aes(x=x,y=y,label=label),
    fill = NA, label.color = NA, family='roboto') +
  geom_richtext(
    aes(x=2020.5,y=7.5,label="**Log of total titles**"),color='grey70',
    fill = NA, label.color = NA, family='roboto'
  ) +
  scale_fill_manual(values = col_pal) +
  scale_color_manual(values = col_pal) +
  geom_point(aes(x=added_year, y=log(cum_year)),
             shape=21, fill=NA,color="grey25",size=16,
             stroke = 1.2)+
  scale_x_continuous(breaks = c(2008:2021)) +
  scale_y_continuous(expand = c(0.05,0), position = 'right')+
  coord_cartesian(clip = 'off') +
  labs(
    caption = "*Visualiation: Tuo Wang | Data Source: Kaggle*") +
  theme_bw()+
  theme(
    text = element_text(family = "roboto", size = 10, color="white"),
    plot.title = element_markdown(family = "roboto", size=20, color="white"),
    plot.subtitle = element_markdown(family = "roboto", size=18, color="white"),
    plot.caption = element_markdown(size=10, color="white"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    rect = element_blank(),
    panel.grid = element_line(color = "grey70"),
    #panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 10, color = "white"),
    axis.title = element_text(size=12),
    axis.title.y = element_blank(),
    axis.text.y = element_text(),
    axis.line.y = element_line(
      arrow = arrow(angle=30,length = unit(0.1, "inches"),type="closed" ),
      size=2, 
      color = "grey70"),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    # axis.ticks.x = element_line(
    #   size = 1.0, color="white",
    #   arrow = arrow(length = unit(0.4, "lines"), ends = "last", type = "closed")
    # ),
    axis.text.x = element_blank(),
    # axis.line = element_line(
    #   arrow = arrow(length = unit(0.7, "lines")),colour = "grey90"),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "#525252"),
    plot.margin = margin(25, 5, 0, 0)
  )

ggsave(
  filename = here::here("2021","2021-04-20","2021-04-20-a.png"),
  width = 10,
  height = 6,
  device = "png"
)

