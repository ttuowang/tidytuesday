# Title: TidyTuesday, 2021-03-23
# Description: Video Games and Sliced
# Author: Tuo Wang
# Date: 2021-03-24

library(tidytuesdayR)
library(tidyverse)
library(here)         # constructs
library(skimr)        # skim the data
library(ggrepel)      # automatically position non-overlapping text labels 
library(showtext)

font_add_google("Merriweather", "Merriweather")
font_add_google("Luckiest Guy", "luckiest")
font_add_google("Poppins", "poppins")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2021-03-23')

unvotes <- tuesdata$unvotes
issues <- tuesdata$issues

skimr::skim(unvotes)

unvotes_df <- unvotes %>%
  select(country, rcid, vote) %>%
  mutate(
    vote = factor(vote, levels = c("no", "abstain", "yes")),
    vote = as.numeric(vote)#,
    #rcid = paste0("rcid_", rcid)
  ) %>%
  pivot_wider(names_from = "rcid", 
    names_prefix = "rcid_",
    values_from = "vote",
    values_fill = 2)

# Principal component analysis
library(recipes)

pca_rec <- recipe(country~., data = unvotes_df) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), num_comp = 5)

pca_prep <- prep(pca_rec)

pca_prep

permanent_country_list <- c(
  "China", "France", "Russia", "United Kingdom", "United States"
)

df <- bake(pca_prep, new_data = NULL) %>%
  mutate(
    country = as.character(country),
    permanent_country = ifelse(
    country %in% permanent_country_list, country, ""),
    non_permanent = ifelse(
      country %in% permanent_country_list, "", country)) 

df %>%
  ggplot(aes(PC1, PC2, label = permanent_country)) +
  geom_point(color = "midnightblue", alpha = 0.7, size = 1.5) +
  # ggrepel::geom_text_repel(
  #   aes(PC1, PC2, label = non_permanent),
  #   family = "Merriweather",size=2, color="grey50",
  #   hjust=1.3,vjust=-2) +
  geom_text(data=filter(df, permanent_country==""),aes(PC1,PC2,label=country),
            check_overlap = TRUE, hjust = "inward",color="grey50")+
  ggrepel::geom_text_repel(
    family = "Merriweather",size=5, color="black",
    hjust=1.3,vjust=-2) +
  geom_point(data=filter(df, permanent_country!=""),aes(PC1,PC2),
             color = "#fa5555", size = 2.5) +
  labs(title="What's the difference between the five permanent members",
       subtitle = "Principal component analysis",
       x="PC1", y="PC2",caption = "Data: TidyTuesday") +
  theme(
    text = element_text(family = "poppins", size = 8, color = "black"),
    plot.title = element_text(family = "poppins", size = 20,
                              face = "bold", color="#2a475e"),
    plot.subtitle = element_text(family = "poppins", size = 15, 
                                 face = "bold", color="#1b2838"),
    plot.caption = element_text(size = 10),
    axis.text = element_text(size = 10, color = "black"),
    rect = element_blank(),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #panel.grid.major.x = element_line(linetype="dashed"),
    panel.grid.major.y = element_blank(),
    #panel.grid.major.y = element_line(linetype="dashed"),
    axis.ticks = element_blank(),
    # axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    #axis.line = element_line(colour = "grey50"),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

ggsave(
  filename = here::here("2021","2021-03-23","2021-03-23-a.png"),
  width = 10,
  height = 10,
  device = "png"
)


library(embed)
umap_rec <- recipe(~., data = unvotes_df) %>%
  update_role(country, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())
umap_prep <- prep(umap_rec)
umap_prep

df2 <- bake(umap_prep, new_data = NULL) %>%
  mutate(
    country = as.character(country),
    permanent_country = ifelse(
      country %in% permanent_country_list, country, ""),
    non_permanent = ifelse(
      country %in% permanent_country_list, "", country)) 

df2 %>%
  ggplot(aes(umap_1, umap_2, label = permanent_country)) +
  geom_point(color = "midnightblue", alpha = 0.7, size = 1.5) +
  # ggrepel::geom_text_repel(
  #   aes(PC1, PC2, label = non_permanent),
  #   family = "Merriweather",size=2, color="grey50",
  #   hjust=1.3,vjust=-2) +
  geom_text(data=filter(df2, permanent_country==""),aes(umap_1,umap_2,label=country),
            check_overlap = TRUE, hjust = "inward",color="grey50")+
  ggrepel::geom_text_repel(
    family = "Merriweather",size=5, color="black",
    hjust=1.3,vjust=-2) +
  geom_point(data=filter(df2, permanent_country!=""),aes(umap_1,umap_2),
             color = "#fa5555", size = 2.5) +
  labs(title="What's the difference between the five permanent members",
       subtitle = "Principal component analysis",
       x="UMAP1", y="UMAP2",caption = "Data: TidyTuesday") +
  theme(
    text = element_text(family = "poppins", size = 8, color = "black"),
    plot.title = element_text(family = "poppins", size = 20,
                              face = "bold", color="#2a475e"),
    plot.subtitle = element_text(family = "poppins", size = 15, 
                                 face = "bold", color="#1b2838"),
    plot.caption = element_text(size = 10),
    axis.text = element_text(size = 10, color = "black"),
    rect = element_blank(),
    panel.grid = element_line(color = "#b4aea9"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    #panel.grid.major.x = element_line(linetype="dashed"),
    panel.grid.major.y = element_blank(),
    #panel.grid.major.y = element_line(linetype="dashed"),
    axis.ticks = element_blank(),
    # axis.text.x = element_blank(),
    # axis.text.y = element_blank(),
    #axis.line = element_line(colour = "grey50"),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )

ggsave(
  filename = here::here("2021","2021-03-23","2021-03-23-b.png"),
  width = 10,
  height = 10,
  device = "png"
)
