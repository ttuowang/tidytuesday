# Title: TidyTuesday, 2021-03-30
# Author: Tuo Wang
# Date: 2021-03-30

library(tidytuesdayR)
library(tidyverse)
library(here)         # constructs
library(skimr)        # skim the data
library(ggdist)
library(ggtext)
library(patchwork)
library(showtext)

font_add_google("Merriweather", "merriweather")
font_add_google("Poppins", "poppins")
font_add_google("Courgette", "courgette")
font_add_google("Roboto", "roboto")

showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2021-03-30')

allShades <- tuesdata$allShades

nudeShades <- allShades %>%
  filter(!is.na(name)) %>%
  filter(str_detect(str_to_lower(name),"nude")) %>%
  mutate(group = "nude") %>%
  arrange(lightness)

naturalShades <- allShades %>%
  filter(!is.na(name)) %>%
  filter(str_detect(str_to_lower(name),"natural")) %>%
  mutate(group = "natural") %>%
  arrange(lightness)

warmShades <- allShades %>%
  filter(!is.na(name)) %>%
  filter(str_detect(str_to_lower(name),"warm")) %>%
  mutate(group = "warm") %>%
  arrange(lightness)

nude_lightness_summary <- data.frame(
  q025 = quantile(nudeShades$lightness,0.025),
  q10 = quantile(nudeShades$lightness,0.10),
  q50 = median(nudeShades$lightness),
  q90 = quantile(nudeShades$lightness,0.9),
  q975 = quantile(nudeShades$lightness,0.975),
  label1 = "80% of shades fall in this range",
  label2 = "95% of shades fall in this range"
)

natural_lightness_summary <- data.frame(
  q025 = quantile(naturalShades$lightness,0.025),
  q10 = quantile(naturalShades$lightness,0.10),
  q50 = median(naturalShades$lightness),
  q90 = quantile(naturalShades$lightness,0.9),
  q975 = quantile(naturalShades$lightness,0.975),
  label1 = "80% of shades fall in this range",
  label2 = "95% of shades fall in this range"
)

warm_lightness_summary <- data.frame(
  q025 = quantile(warmShades$lightness,0.025),
  q10 = quantile(warmShades$lightness,0.10),
  q50 = median(warmShades$lightness),
  q90 = quantile(warmShades$lightness,0.9),
  q975 = quantile(warmShades$lightness,0.975),
  label1 = "80% of shades fall in this range",
  label2 = "95% of shades fall in this range"
)

shade_text <- data.frame(
  label1=c("98 shades with <span style='color:#159090'>**\"nude\"**</span> in the name"),
  label2=c("135 shades with <span style='color:#386cb0'>**\"natural\"**</span> in the name"),
  label3=c("401 shades with <span style='color:#A034F0'>**\"warm\"**</span> in the name"),
  x = 0.095,
  y = -0.17
)

nude_arrows <-
  tibble(
    x1 = c(0.25),
    x2 = c(0.19),
    y1 = c(0.35),
    y2 = c(0.08),
    label1 = "Brand: *Milani*<br>
               Product: *Screen Queen Foundation*<br>
               Name: <span style='color:#159090'>*Nude*</span> *Mocha*"
  )

natural_arrows <-
  tibble(
    x1 = c(0.34),
    x2 = c(0.28),
    y1 = c(0.35),
    y2 = c(0.08),
    label1 = "Brand: *Dermablend*<br>
               Product: *Leg and Body Makeup*<br>
               Name: *Deep* <span style='color:#386cb0'>*Natural*</span>"
  )

warm_arrows <-
  tibble(
    x1 = c(0.25),
    x2 = c(0.20),
    y1 = c(0.35),
    y2 = c(0.08),
    label1 = "Brand: *Morphe*<br>
               Product: *Fluidity Full-Coverage Foundation*<br>
               Name: *Deep* <span style='color:#A034F0'>*Warm*</span>"
  )

tt <- data.frame(
  x = 0,
  y = 0.1,
  label1 = "Nude",
  label2 = "Natural",
  label3 = "Warm"
)

lines <- data.frame(
  x = c(1),
  y = c(-0.050),
  xend = c(0),
  yend = c(-0.050),
  width = c(1),
  colour = "#392928"
)


#library(ggforce)
p1 <- nudeShades %>%
  ggplot() +
  geom_dots(aes(x=lightness,fill=hex, group=1),
            shape=22, color="white",show.legend = FALSE) +
  # geom_segment(aes(x=0,xend=1.07,y=0,yend=0),
  #              arrow = arrow(length = unit(0.03, "npc")),
  #              color="grey50") +
  # geom_segment(
  #   data = nude_lightness_summary,color="grey40",
  #   aes(x=q10,xend=q90,y=0,yend=0),size=1.5)+
  # geom_point(data = nude_lightness_summary,aes(x=q50,y=0),size=2,
  #            color="grey30") +
  scale_fill_manual(values = nudeShades$hex) +
  geom_link(
    data = lines,
    aes(x = x, y = y, xend = xend, yend = yend, 
                alpha = stat(index)), show.legend = FALSE,
    size=4,colour = "#824E2B") +
  scale_colour_gradient(low="#824E2B", high="#FDFBFA")+
  # geom_rect(data = nude_lightness_summary,
  #           aes(xmin=q10,xmax=q90,ymin=-0.08,ymax=-0.03), fill="grey90")+
  # geom_text(data = nude_lightness_summary,family = "roboto",
  #           aes(x=q50-0.015,y=-0.065,label=label1),size=3.0) +
  # annotate("text", x=1.04,y=-0.065,label="lightness",family="roboto",
  #          size=3.0)+
  geom_richtext(
    data=shade_text,aes(x=x,y=y,label=label1),family="roboto",
    size=3.0,fill = NA, label.color=NA) +
  # geom_curve(
  #   data = nude_arrows, 
  #   aes(x=x1,y=y1,xend=x2,yend=y2),color = "gray20", curvature = 0.3,
  #   arrow = arrow(length = unit(0.07, "inch")), size = 0.4) +
  # geom_richtext(
  #   data = nude_arrows,
  #   aes(x=x1,y=y1,label=label1),family="roboto",hjust=0,vjust=0.2,
  #   size=3.0,fill = NA, label.color=NA)+
  geom_text(
    data = tt,
    aes(x=x, y=y, label=label1),
    size = 15,
    alpha=0.1,
    family="roboto",
    color = "#159090",
    fontface = "bold",
    vjust = 0.1,
    hjust = 0.03
  ) +
  coord_cartesian(expand = FALSE, clip = 'off')+
  ylim(c(-0.17,1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "poppins", size = 8, color = "black"),
    plot.title = element_text(family = "poppins", size = 20,
                              face = "bold", color="#2a475e"),
    plot.subtitle = element_text(family = "poppins", size = 15, 
                                 face = "bold", color="#1b2838"),
    plot.caption = element_text(size = 10),
    rect = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "white", color = "white")
  )


p2 <- naturalShades %>%
  ggplot() +
  geom_dots(aes(x=lightness,fill=hex, group=1), size=0.1,
            shape=22, color="white",show.legend = FALSE) +
  # geom_segment(aes(x=0,xend=1.07,y=0,yend=0),
  #              arrow = arrow(length = unit(0.03, "npc")),
  #              color="grey50") +
  # geom_segment(
  #   data = natural_lightness_summary,color="grey40",
  #   aes(x=q10,xend=q90,y=0,yend=0),size=1.5)+
  # geom_point(data = natural_lightness_summary,aes(x=q50,y=0),size=2,
  #            color="grey30")+
  scale_fill_manual(values = naturalShades$hex) +
  geom_link(
    data = lines,
    aes(x = x, y = y, xend = xend, yend = yend, 
        alpha = stat(index)), show.legend = FALSE,
    size=4,colour = "#824E2B") +
  scale_colour_gradient(low="#824E2B", high="#FDFBFA")+
  # geom_rect(data = natural_lightness_summary,
  #           aes(xmin=q10,xmax=q90,ymin=-0.08,ymax=-0.03), fill="grey90")+
  # geom_text(data = natural_lightness_summary,family = "roboto",
  #           aes(x=q50+0.015,y=-0.065,label=label1),size=3.0) +
  # annotate("text", x=1.04,y=-0.065,label="lightness",family="roboto",
  #          size=3.0)+
  geom_richtext(
    data=shade_text,aes(x=x+0.01,y=y,label=label2),family="roboto",
    size=3.0,fill = NA, label.color=NA) +
  # geom_curve(
  #   data = natural_arrows, 
  #   aes(x=x1,y=y1,xend=x2,yend=y2),color = "gray20", curvature = 0.3,
  #   arrow = arrow(length = unit(0.07, "inch")), size = 0.4) +
  # geom_richtext(
  #   data = natural_arrows,
  #   aes(x=x1,y=y1,label=label1),family="roboto",hjust=0,vjust=0.2,
  #   size=3.0,fill = NA, label.color=NA)+
  coord_cartesian(expand = FALSE, clip = 'off')+
  geom_text(
    data = tt,
    aes(x=x, y=y, label=label2),
    size = 15,
    alpha=0.1,
    family="roboto",
    color = "#386cb0",
    fontface = "bold",
    vjust = 0.1,
    hjust = 0.03
  ) +
  ylim(c(-0.17,1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "roboto", size = 8, color = "black"),
    plot.title = element_text(family = "roboto", size = 20,
                              face = "bold", color="#2a475e"),
    plot.subtitle = element_text(family = "roboto", size = 15, 
                                 face = "bold", color="#1b2838"),
    plot.caption = element_text(size = 10),
    rect = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "white", color = "white")
  )



p3 <- warmShades %>%
  ggplot() +
  geom_dots(aes(x=lightness,fill=hex, group=1), size=0.1,
            shape=22, color="white",show.legend = FALSE) +
  # geom_segment(aes(x=0,xend=1.07,y=0,yend=0),
  #              arrow = arrow(length = unit(0.03, "npc")),
  #              color="grey50") +
  # geom_segment(
  #   data = warm_lightness_summary,color="grey40",
  #   aes(x=q10,xend=q90,y=0,yend=0),size=1.5)+
  # geom_point(data = warm_lightness_summary,aes(x=q50,y=0),size=2,
  #            color="grey30")+
  geom_link(
    data = lines,
    aes(x = x, y = y, xend = xend, yend = yend, 
        alpha = stat(index)), show.legend = FALSE,
    size=4,colour = "#824E2B") +
  scale_colour_gradient(low="#824E2B", high="#FDFBFA")+
  scale_fill_manual(values = warmShades$hex) +
  # geom_text(data = warm_lightness_summary,family = "roboto",
  #           aes(x=q50,y=-0.065,label=label1),size=3.0) +
  # annotate("text", x=1.04,y=-0.065,label="lightness",family="roboto",
  #          size=3.0)+
  geom_richtext(
    data=shade_text,aes(x=x+0.005,y=y,label=label3),family="roboto",
    size=3.0,fill = NA, label.color=NA) +
  geom_text(
    data = tt,
    aes(x=x, y=y, label=label3),
    size = 15,
    alpha=0.1,
    family="roboto",
    color = "#A034F0",
    fontface = "bold",
    vjust = 0.1,
    hjust = 0.03
  ) +
  # geom_curve(
  #   data = warm_arrows, 
  #   aes(x=x1,y=y1,xend=x2,yend=y2),color = "gray20", curvature = -0.3,
  #   arrow = arrow(length = unit(0.07, "inch")), size = 0.4) +
  # geom_richtext(
  #   data = warm_arrows,
  #   aes(x=x1,y=y1,label=label1),family="roboto",hjust=0,vjust=0.2,
  #   size=3.0,fill = NA, label.color=NA)+
  coord_cartesian(expand = FALSE, clip = 'off')+
  ylim(c(-0.17,1)) +
  theme_minimal() +
  theme(
    text = element_text(family = "roboto", size = 8, color = "black"),
    plot.title = element_text(family = "roboto", size = 20,
                              face = "bold", color="#2a475e"),
    plot.subtitle = element_text(family = "roboto", size = 15, 
                                 face = "bold", color="#1b2838"),
    plot.caption = element_text(size = 10),
    rect = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill = "white", color = "white")
  )

p1 / p2 /p3 +
  plot_annotation(
    title = "Distribution of shades with <span style='color:#159090'>**\"nude\"**</span>, 
    <span style='color:#386cb0'>**\"natural\"**</span> and 
    <span style='color:#A034F0'>**\"warm\"**</span> in the name",
    subtitle = "*Shades ordered by lightness*",
    caption = "*Data Source: The Pudding*") &
  theme(
    text = element_text(family = "roboto", size = 8, color = "black"),
    plot.title = element_markdown(family = "roboto", size = 20,
                              face = "bold", color="black"),
    plot.subtitle = element_markdown(family = "roboto", size = 15, 
                                     color="black"),
    plot.caption = element_markdown(size = 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(10,10,10,10),
  )

ggsave(
  filename = here::here("2021","2021-03-30","2021-03-30-d.png"),
  width = 10,
  height = 6,
  device = "png"
)







