
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue)


# Loading data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-12-12")
movies <- tuesdata$holiday_movies
genres <-  tuesdata$holiday_movie_genres


# Loading fonts ------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Fira code", "firac")
font_add_google("Fira sans", "firas")
font_add_google("Fira mono", "firam")
showtext_auto()


# Defining colors ---------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Wrangling Data ----------------------------------------------------------

## EDA
movies |> glimpse()
movies |> count(title_type, sort = TRUE)


movies$average_rating |> hist()
movies$runtime_minutes |> hist()

movies |> 
  filter(!is.na(runtime_minutes)) |> 
  ggplot(aes(average_rating, runtime_minutes)) +
  geom_point()


genres |> count(genres, sort = TRUE)
movies |> 
  select(genres) |> 
  separate_rows(genres) |> 
  count(genres, sort = TRUE)


## holiday type
movies |> count(holiday, sort = TRUE)

by_types <- movies |>
  select(christmas, hanukkah, kwanzaa, holiday) |> 
  summarise(
    christmas =  sum(christmas), 
    hanukkah =  sum(hanukkah),
    kwanzaa =  sum(kwanzaa), 
    holiday =  sum(holiday)) |> 
  pivot_longer(everything(), 
               names_to = 'type', 
               values_to = 'count')




# Define texts and annotations --------------------------------------------

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


# Data Viz -------------------------------------------------------------------



## Movies title type by year ----------
movies |> 
  count(year) |> 
  ggplot(aes(year, n)) +
  geom_line()

min(movies$year)
movies |> 
  drop_na() |> 
  count(year, title_type) |> 
  mutate(title_type = str_to_upper(title_type)) |> 
  ggplot(aes(year, n, fill  = title_type)) +
  geom_col(width = 1) +
  geom_text(
    aes(x = 1930, y = 50, label =  'VIDEOS'), 
    color = '#D62828',
    hjust = 0, vjust = 0) +
  annotate(
    "text", label = "Started being produced around the 80s\nwith a pick between 2000 and 2010",
    x = 1930, y = 35, size = rel(5), colour = "firebrick", hjust = 0, vjust = 0) +
  geom_text(
    aes(x = 1950, y = 100, label =  'MOVIES'), 
    color = 'skyblue',
    hjust = 0, vjust = 0) +
  annotate(
    "text", label = "Started first around 1929-1930\nand expanded after 2010",
    x = 1950, y = 85, size = rel(5), colour = "skyblue", hjust = 0, vjust = 0)  +
  geom_text(
    aes(x = 1975, y = 150, label =  'TVMOVIES'), 
    color = '#FCB322',
    hjust = 0, vjust = 0) +
  annotate(
    "text", label = "Started around around 1950\nwith a pick after 2010",
    x = 1975, y = 135, size = rel(5), colour = "wheat", hjust = 0, vjust = 0) +
  scale_y_continuous(expand = c(0, 2),
                     breaks = c(50, 100, 150)) + 
  theme_fivethirtyeight() +
  guides(fill = guide_legend(label.position = 'top',
                             keywidth = 4,
                             keyheight = .5,
                             color = 'red')) +
  scale_fill_manual(
    values = c("MOVIE" = "#004266", 
               "TVMOVIE" = "#FCB322", 
               "VIDEO" = "#D62828")) +
  theme(
    plot.title = element_text(
      size = 25,
      color = 'white',
      family = 'firas',
      face = 'bold'),
    plot.subtitle = element_text(
      size = 18,
      color = '#0a9396',
      family = 'firac',
      margin = margin(0,0,1.5,0, unit = 'cm')),
    legend.position = c(.5, 1),
    legend.title = element_blank(),
    legend.text = element_text(
      color = 'white', 
      face = 'bold'),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.key = element_rect(colour = 'black', size = 2, linetype = 1),
    panel.grid = element_line(linewidth = 0),
    axis.title.y = element_blank(),
    axis.line = element_line(color = 'grey90', linewidth = .1),
    axis.text = element_text(
      color   = 'grey50', 
      size = 16),
    plot.background = element_rect(fill = 'black'),
    panel.background = element_rect(fill = 'black')) +
  labs(
    title = "Holiday Movies",
    subtitle = "From 1925 to 2023") -> my_year
  
my_year

# Saving Plots and Gifs ------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-12-12", paste0("20231212", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )

library(Cairo)
getwd()
ggsave(my_year,
       file = "movie_.png",  type = "cairo-png",  dpi = 300
       )
