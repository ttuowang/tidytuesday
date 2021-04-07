# Title: TidyTuesday, 2021-04-06
# Author: Tuo Wang
# Date: 2021-04-06

library(tidytuesdayR)
library(tidyverse)
library(here)         
library(showtext)
library(ggtext)
library(ggforce)

font_add_google("Roboto", "roboto")
font_add_google("Roboto Slab", "roboto slab")

showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2021-04-06')

forest <- tuesdata$forest
forest_area <- tuesdata$forest_area
brazil_loss <- tuesdata$brazil_loss
soybean_use <- tuesdata$soybean_use
vegetable_oil <- tuesdata$vegetable_oil

a <- forest_area %>%
  filter(year == 2000, !is.na(code)) %>%
  arrange(desc(forest_area)) %>%
  slice(2:n()) %>%
  top_n(forest_area, n=10)

region_lst <- c(
  "Asia", "Europe", "Africa", "Northern America", "South America", "Oceania", 
  a$entity
)

b <- forest_area %>%
  filter(entity %in% region_lst, year %in% c(2000, 2020)) %>%
  select(-code) %>%
  pivot_wider(
    names_from = year, names_prefix = "year", values_from = forest_area) %>%
  mutate(rank = rank(-year2020),
         diff = case_when(
           year2020 > year2000 ~ "A",
           year2020 < year2000 ~ "B",
           year2020 == year2000 ~ "C"
         ),
         name = case_when(
           diff == "A" ~ paste0("<span style='color:#FF8C00'>",entity,"</span>"),
           diff == "B" ~ paste0("<span style='color:#386cb0'>",entity,"</span>"),
           diff == "C" ~ paste0("<span style='color:#A034F0'>",entity,"</span>")
         )) %>%
  arrange(-year2020)



b %>%
  ggplot() +
  geom_link(
    aes( x = year2000, y=rank,
         xend = year2020, yend=rank,
         color=diff,
         size = stat(index)),
    show.legend = FALSE,
    n = 1000
  ) +
  geom_point(
    aes(year2020,rank,color=diff),
    show.legend = FALSE,
    shape=21, fill="white",size=5
  ) +
  scale_color_manual(values = c("#FF8C00","#386cb0")) +
  scale_x_continuous(
    expand = c(0.1, 0.1),
    breaks = c(0,5,10,15,20,25),
    labels = paste0(c(0,5,10,15,20,25),"%"),
    sec.axis = dup_axis()) +
  scale_y_reverse(
    breaks = 1:nrow(b), 
    labels = b$name
  ) +
  geom_richtext(
    aes(
      x=17.5,y=13,label = "<b style='font-size:10pt;'>How are forests distributed across the world regions from 2000 to 2020</b> <br> 
      <br><span style='font-size:10pt;'><span style='color:#386cb0'>Decreasing 2020 < 2000</span> <span style='color:#FF8C00'>Increasing 2020 > 2000</span> <br>
      <br><span style='font-size:8pt;'>The top 10 countries with the highest forest share in 2000 are: Russia, Brazil, Canada, <br>
      United States, China, Australia, Democratic Republic of Congo, Indonesia, Peru, Angola."),
    family = "roboto",
    size = 1.5,
    label.padding = unit(1.5, "lines"),
    label.size = 1,
    label.color = "grey70"
  )+
  geom_richtext(
    data = data.frame(
      x = c(9.3, 15.8), y = 7, 
      label = c("Percentage in 2020 ->", "<- Percentage in 2000")
    ),
    aes(x, y, label = label),
    family = "roboto",
    fontface = "bold",
    size = 3.5, 
    color = "grey50",
    fill = NA, 
    label.color = NA
  ) +
  labs(caption = "*Data Source: Our World in Data*")+
  theme_minimal() +
  theme(
    text = element_text(family = "roboto", size = 10),
    plot.title = element_markdown(family = "roboto", size = 20,
                                  face = "bold", color="black"),
    plot.subtitle = element_markdown(family = "roboto", size = 15, 
                                     color="black"),
    axis.text.y = element_markdown(margin = margin(r=10),size = 10,
                                   family = "roboto", face = "bold"),
    plot.caption = element_markdown(size = 10),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x.top  = element_text(size = 10, face = "bold", vjust = 1.0),
    axis.text.x.bottom   = element_text(size = 10, face = "bold", vjust = .15),
    axis.ticks.x.top  = element_line(
      size = 1.0,
      arrow = arrow(length = unit(0.4, "lines"), ends = "first", type = "closed")
    ),
    axis.ticks.x.bottom = element_line(
      size = 1.0,
      arrow = arrow(length = unit(0.4, "lines"), ends = "last", type = "closed")
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "white", color = "white")
  )


ggsave(
  filename = here::here("2021","2021-04-06","2021-04-06-a.png"),
  width = 10,
  height = 6,
  device = "png"
)

col_palette <- c("#FF8C00", "#A034F0", "#159090")
values = c("#386cb0","#fdb462")








################################################################################
# forest_area %>%
#   filter(year == 2020) %>%
#   View()
# 
# 
continent <- c(
  "Asia", "Europe", "Africa", "North America", "South America", "Oceania"
)

soybean_use_global <- soybean_use %>%
  filter(entity %in% continent) %>%
  group_by(year) %>%
  summarise(
    human_food_global = sum(human_food),
    animal_feed_global = sum(animal_feed),
    processed_global = sum(processed)) %>%
  ungroup() %>%
  mutate(
    total_global = human_food_global+animal_feed_global+processed_global) %>%
  mutate(
    human_percent = human_food_global/total_global,
    animal_percent = animal_feed_global/total_global,
    processed_percet = processed_global/total_global)

b <- soybean_use_global %>%
  select(year, human_percent, animal_percent) %>%
  pivot_longer(!year, names_to = "usage", values_to = "howmuch")

ggplot(b, aes(x=1, y=howmuch, fill=usage))+
  geom_col(position = position_stack())

library(gganimate)
anim <- ggplot(b, aes(x=1, y=howmuch,fill=usage))+
  geom_col() +
  transition_time(year)

anim_save("test.gif", anim)

ggsave(
  filename = here::here("2021","2021-04-06","2021-04-06-a.png"),
  width = 10,
  height = 6,
  device = "png"
)