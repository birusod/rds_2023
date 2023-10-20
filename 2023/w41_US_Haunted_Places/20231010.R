
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, 
  showtext, patchwork, ggtext, glue, ggalt,
  sf, usmap, spData)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-10")

hp <- tuesdata$haunted_places |> 
  select(city, state, state_abbrev, longitude, latitude)
# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------
pal_map <- c("#bb3e03","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#005f73")
bg_map <- "#001219"
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------

hp |> filter(state == 'Alaska')
by_state |> filter(state == 'Alaska')
by_state <- 
  hp |> count(state, name = 'total') |> 
  mutate(state = fct_reorder(state, total))

hp |> 
  filter(longitude < -50) |> 
  ggplot(aes(longitude, latitude)) +
  geom_point(alpha = .4) +
  ggthemes::theme_map()


# sf
#library(spData)
data(us_states)
map_df1 <- 
  us_states |> 
  select(state = NAME) |> 
  left_join(by_state) |> 
  mutate(class = case_when(
    total < 200 ~ '1',
    total < 400 ~ '2',
    total < 600 ~ '3',
    total < 800 ~ '4',
    TRUE ~ '5'
  ))

# usmap
map_df2 <- 
  statepop |> 
  left_join(by_state, join_by(full == state))







# Define text -------------------------------------------------------------

# social <- nrBrand::social_caption(
#   bg_colour = bg_col,
#   icon_colour = highlight_col,
#   font_colour = text_col,
#   font_family = "roboto"
# )
title <- ""
st <- ""
cap <- paste0(
  "**Data**: <br>", #social
)


# Plot --------------------------------------------------------------------

by_state |> 
  ggplot(aes(state, total))  +
  geom_lollipop(color = '#b2435f', point.colour = '#fb8654',
                size = 1) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1100)) +
  labs(title = 'Hounted  Places By  State  - USA',
       x = '', y = '') +
  theme_light() +
  theme(
    plot.title = element_text(
      family = 'roboto', face = 'bold', 
      color = '#8179dd', size = 20,
      hjust = .5),
    panel.grid.major.y = element_line(linetype = 'dotted', linewidth = .05),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = 'grey30'),
    axis.text = element_text(color = 'white'),
    axis.text.x = element_text(size = 14),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = '#3D3236'),
    plot.background = element_rect(fill = 'black')
  ) -> p1


map_df1 |> 
  ggplot(aes(fill = total)) +
  geom_sf() +
  scale_fill_gradient2(
    low = 'orange', 
    mid = 'white',
    high = 'darkred',
    midpoint = 600) +
  labs(title = "Hounted Places By State in USA") +
  ggthemes::theme_map()


# Code by: https://r-graph-gallery.com/web-choropleth-map-lego-style.html
map_df1  |> 
  ggplot(
    aes(fill = class)) +
  geom_sf(color  = 'grey') +
  guides(
    fill = guide_legend(
      nrow = 1,
      title = 'Number of haunted places',
      title.position = "top",
      label.position = "bottom",
      keywidth = 3)
  ) +
  scale_fill_manual(
    values = pal_map,
    label = c("<200","200-400","400-600","600-800","1000+")) +
  labs(title = "Hounted Places By State in USA") +
  theme_void() +
  theme(
    plot.title = element_text(family = 'roboto', color =  'darkorange', 
                              size = 20, hjust = .5, face = 'bold'),
    plot.margin = margin(1,1,10,1,"pt"),
    plot.background = element_rect(fill = bg_map,color = NA),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5,color = "white",face = "bold"),
    legend.text = element_text(color = "white")
  )


# usmap
plot_usmap(regions = "states")

plot_usmap(data = map_df2, 
           values = "total", 
           color = "grey",
           labels = TRUE) + 
  guides(
    fill = guide_legend(
      nrow = 1,
      title.position = "top",
      title.hjust = .5,
      label.position = "bottom",
      keywidth = 3)
  ) + 
  #scale_fill_gradientn(colours = colorspace::heat_hcl(7))
  scale_fill_gradient2(
    low = "#FFFFFF",  
    mid = '#E2E6BD',
    high = "#D33F6A", 
    midpoint = 625,
    name = "Number of haunted places", label = scales::comma) + 
  labs(title = "Hounted Places By State in USA") +
  theme(
    legend.position = c(.5, 0),
    plot.title = element_text(family = 'roboto', color =  'darkblue', 
                                size = 20, hjust = .5, face = 'bold'),
    plot.margin = margin(1,1,10,1,"pt"),
    legend.title = element_text(hjust = 0.5,color = "black",face = "bold"),
    legend.text = element_text(face = "bold")
    )


# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-10-10", paste0("20231010", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
