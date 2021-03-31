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



# Code from
# https://github.com/gkaramanis/tidytuesday/blob/master/2021/2021-week12/unvotes-bar.R
# library(tidyverse)
# library(vhs)
# 
# tuesdata <- tidytuesdayR::tt_load('2021-03-23')
# 
# issues <- tuesdata$issues
# 
# issues_count <- issues %>% 
#   count(issue) %>% 
#   mutate(issue = fct_rev(issue))
# 
# pal <- vhs("recoton")
# 
# # ggplot(issues) +
# #   geom_bar(aes(x=issue, fill=issue))
# 
# font_add_google("Chango", "chango")
# showtext_auto()
# 
# ggplot(issues_count) +
#   geom_col(aes(x = n - 100, y = issue, fill = issue)) +
#   # Letters
#   geom_text(aes(x = n-30, y = issue, label = str_sub(issue, 1, 1), color = issue),
#             family = "chango",stat = "unique", hjust = 1,size = 43, nudge_x = 18) +
#   # Issue labels
#   geom_text(aes(x = 50, y = issue, label = toupper(issue)), stat = "unique",
#             hjust = 0, color = "grey97", nudge_y = -0.325, size = 3.5) +
#   # Issue Number
#   geom_text(aes(x = 40, y = issue, label = n), stat = "unique",
#             hjust = 0, color = "grey97", size = 28,  nudge_y = 0.04) +
#   # # Hide the ugly stuff
#   # geom_tile(aes(x = n/2, y = issue, width = n, height = 0.925),
#   #           fill = NA, color = "grey25", size = 3) +
#   # Scales, theme, etc.
#   #scale_color_manual(values = pal) +
#   #scale_fill_manual(values = pal) +
#   coord_cartesian(expand = FALSE) +
#   labs(
#     title = "UN roll call votes by issue",
#     caption = "Source: Harvard's Dataverse · Graphic: Georgios Karamanis") +
#   theme_void() +
#   theme(
#     legend.position = "none",
#     plot.background = element_rect(fill = "grey25", color = NA),
#     plot.title = element_text(margin = margin(0, 0, 10, 0), family = "Produkt Medium", color = "grey97", size = 30, hjust = 0.5),
#     plot.caption = element_text(margin = margin(10, 0, 0, 0), hjust = 1, family = "Produkt", color = "grey70", size = 7),
#     plot.margin = margin(20, 20, 10, 20)
#   )
# 
# 
# ggsave(
#   filename = here::here("2021","2021-03-23","test.png"),
#   width = 10,
#   height = 10,
#   device = "png"
# )
