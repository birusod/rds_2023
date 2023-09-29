
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, 
  showtext, patchwork, ggtext, glue, cowplot, patchwork,
  sf, rnaturalearth, rnaturalearthdata, ggthemes,
  camcorder)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-09-12")
tuesdata

all_countries <- tuesdata$all_countries
regions <- tuesdata$country_regions
ghd <- tuesdata$global_human_day
gea <- tuesdata$global_economic_activity

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()
showtext_auto(showtext_opts(dpi = 300))

# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------
all_countries |> head(10) |> view()
regions |> head(10) |> view()
ghd |> head(10) |> view()
gea |> head(10) |> view()

# global economic activity
#mytitle <- "<span style = 'color:#E69F00;'>Percent Of Employed Workers Who Are</span>
#  <i style = 'color:#009E73;'>Union Members</i>
#  <span style = 'color:#CC79A7;'>in 2022</span>"

#cap <- paste0("*TidyTuesday Data | Week-36*")


# global human day
ghd |> arrange(hoursPerDay)

ghd <- ghd |> 
  mutate(
    Subcategory  = fct_lump_min(
      Subcategory, min = .5, w = hoursPerDay) |> fct_reorder(hoursPerDay),
    clr = if_else(Subcategory == 'Other', 'no', 'yes')) 



gea |> arrange(hoursPerDay)
gea <- gea |> 
  mutate(
    Subcategory = fct_lump_n(
      Subcategory, 10, w = hoursPerDay) |> fct_reorder(hoursPerDay),
    clr = if_else(Subcategory == 'Other', 'no', 'yes')) 


# world data for map
world_data <- 
  ne_countries(scale = "medium", returnclass = "sf") |> 
  select(iso_a3, name, name_long, formal_en, formal_fr, 
         economy, income_grp, pop_est, gdp_md, continent)

cdf <- 
  all_countries |> 
  select(country_iso3, Category, Subcategory, hoursPerDayCombined) 

#  merge and select African countries
afr <- 
  world_data |>
  filter(continent  == "Africa") |> 
  left_join(cdf, by = join_by(iso_a3  == country_iso3)) |> 
  group_by(Subcategory) |> 
  mutate(
    # (var - mean) / sd
    hpd = (hoursPerDayCombined  - mean(hoursPerDayCombined, na.rm = TRUE)) / 
      sd(hoursPerDayCombined, na.rm = TRUE),
    # 0 to 1 scale
    hpd2 = rescale(hoursPerDayCombined)) 
# Define text -------------------------------------------------------------

# social <- nrBrand::social_caption(
#   bg_colour = bg_col,
#   icon_colour = highlight_col,
#   font_colour = text_col,
#   font_family = "roboto"
# )
title <- ""
st <- ""
cap <- 'TidyTuesday Data | Week-37'
#paste0("**Data**: <br>", #social)


# Plot --------------------------------------------------------------

p1 <- 
  ghd |> 
  ggplot(aes(hoursPerDay, Subcategory, fill = clr)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(2.5, 5, 7.5),
                     limits = c(0, 9.5)) +
  scale_fill_manual(values = c('grey70', 'steelblue')) +
  labs(y = '', x = '', 
       title = 'Global Human Day',
       subtitle = 'The number of hours per day engaged in each activity averaged across all humans',
       caption  = cap)  +
  theme_light(base_family = 'roboto') +
  theme(
    plot.title = element_text(face = 'bold', size = 16),
    plot.subtitle = element_text(face = 'bold', size = 10),
    axis.text = element_text(face = 'bold', size = 10),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
p1

p2 <- 
  gea |> 
  ggplot(aes(hoursPerDay, Subcategory, fill = clr)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(0.2, 0.4, 0.6),
                     limits = c(0, .75)) +
  scale_fill_manual(values = c('grey70', 'dodgerblue')) +
  labs(y = '', x = 'Hours per day', 
       title = 'Global Economic Activity',
       subtitle = 'Time Devoted to the Global Economy',
       caption  = cap) +
  theme_light(base_family = 'roboto') +
  theme(
    plot.title = element_text(face = 'bold', size = 16),
    plot.subtitle = element_text(face = 'bold', size = 14),
    axis.text = element_text(face = 'bold', size = 10),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )
p2


# Map: African countries

# social:
afr |> 
  filter(Subcategory == 'Social') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  scale_fill_gradient2(
    low = 'darkorange', mid = 'wheat', high = 'firebrick',
    midpoint = 1.15,
    na.value = "grey70",
    guide = guide_legend(
      title = element_blank(),
      keywidth = unit(.5, 'cm'),
      keyheight = unit(.3, 'cm'))) +
  theme_map() +
  theme(legend.position = c(.2, .3),
        plot.title = element_text(hjust = .5, face = 'bold', size = 16),
        legend.text = element_text(size = 12)) +
  labs(title = 'SOCIAL') -> s_plot

# Religious practice
afr |> 
  filter(Subcategory == 'Religious practice') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  scale_fill_gradient2(
    low = 'magenta', mid = 'azure', high = 'navyblue',
    midpoint = .4,
    na.value = "grey70",
    guide = guide_legend(
      title = element_blank(),
      keywidth = unit(.5, 'cm'),
      keyheight = unit(.3, 'cm'))) +
  theme_map() +
  theme(legend.position = c(.2, .3),
        plot.title = element_text(hjust = .5, face = 'bold', size = 16),
        legend.text = element_text(size = 12)) +
  labs(title = 'RELIGION') -> r_plot

# Hygiene & grooming  
afr |> 
  filter(Subcategory == 'Hygiene & grooming') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  scale_fill_gradient2(
    low = '#77ccff', high = '#0044ff',
    midpoint = 0.95,
    na.value = "grey70",
    guide = guide_legend(
      title = element_blank(),
      keywidth = unit(.5, 'cm'),
      keyheight = unit(.3, 'cm'))) +
  theme_map() +
  theme(legend.position = c(.2, .3),
        plot.title = element_text(hjust = .5, face = 'bold', size = 16),
        legend.text = element_text(size = 12)) +
  labs(title = 'HYGIENE/GROOMING')  -> h_plot

# Meals 

afr |> 
  filter(Subcategory == 'Meals') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  scale_fill_gradient2(
    low = '#F4A261' , mid = 'azure', high = '#2A9D8F',
    midpoint = 1.6,
    na.value = "grey70",
    guide = guide_legend(
      title = element_blank(),
      keywidth = unit(.5, 'cm'),
      keyheight = unit(.3, 'cm')
      )
    ) +
  theme_map() +
  theme(legend.position = c(.2, .2),
        plot.title = element_text(hjust = .5, face = 'bold', size = 16),
        legend.text = element_text(size = 12)) +
  labs(title = 'MEALS') -> m_plot


# faceting after rescaling:
afr |> 
  filter(Subcategory %in% c('Social', 'Religious practice', 
                            'Hygiene & grooming', 'Meals')) |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hpd)) +
  facet_wrap(~Subcategory, ncol = 4) +
  scale_fill_gradient2(
    low = '#F4A261' , mid = 'azure', high = '#2A9D8F',
    midpoint = 0,
    guide = guide_legend(
      title = element_blank(),
      label.position = 'top',
      keywidth = unit(1, 'cm'),
      keyheight = unit(.3, 'cm'),
      override.aes = list(color = NA))) +
  theme_map() +
  theme(
    text = element_text(color = 'red'),
    legend.direction = 'horizontal', 
    legend.background = element_blank(),
    legend.position = c(.2, .005),
    strip.background = element_blank(),
    strip.text = element_text(color = 'dodgerblue'),
    plot.background = element_rect(fill = 'black'),
    panel.background = element_rect(fill = 'black'),
    plot.margin = margin(t = 4, b = 0)) -> pw
pw2 <- pw + theme(legend.position = "none")
le1 <- cowplot::get_legend(pw)
cowplot::plot_grid(pw2, le1, nrow = 2, rel_heights = c(6, 1))
pw3 <- pw2 + plot_annotation(
  title = 'The Global Human Day',
  subtitle = 'Hours per day combined for the country by  activity',
  caption = 'TidyTuesday | Week-37',
  theme = theme(
    plot.title = element_text(size = 18, hjust = .5, color = '#999999'),
    plot.subtitle = element_text(size = 14, hjust = .5, color = '#999999'))) +
  theme(text = element_text('roboto'))

ggdraw() +
  draw_plot(pw3) +
  draw_plot(le1, y = .1, x = .15)   -> final_plot

# Save gif -----------------------------------------------------------

pdir <- '/Users/birusod/Documents/DataScienceDocs/GitProjects/R4DS/rds_2023/2023/w37_GHD/Plots_w37'

gg_record(
  dir = file.path(pdir),
  device = "png",
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)

#final_plot

record_polaroid()
gg_stop_recording()

ggsave(paste0(pdir, '/final_plot_w37.png'))


