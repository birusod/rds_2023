
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, 
  showtext, patchwork, ggtext, glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-09-26")
tuesdata

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------

df <- tuesdata$richmondway |> 
  select(order = Episode_order, 
         season = Season,
         episode = Episode,
         dating = Dating_flag,
         coaching = Coaching_flag,
         total = F_count_total
         ) |> 
  mutate(across(-total, \(x) as_factor(x)))

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

df |> 
  ggplot(aes(episode, total)) +
  geom_col() +
  facet_wrap(~ dating + coaching)

df |> 
  group_by(dating, coaching) |> 
  summarise(total = sum(total)) |> 
  ggplot(aes(dating, total, fill = coaching)) +
  geom_col(position = 'fill')

df |> 
  ggplot(aes(order, total, group = 1)) +
  geom_col(fill = '#2b4861', color = NA) +
  labs(x = 'Episodes',
       y = 'Count of F-bombs',
       title = 'TED  LASSSO',
       subtitle = 'Roy Kent F-bomb count by episod',
       caption = 'TidyTuesday | Week-39') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 48)) +
 theme(
   plot.title = element_text(family = 'roboto', size = rel(5), hjust = .5,
                             color = '#0c1125', face = 'bold'),
   plot.subtitle = element_text(family = 'roboto', size = rel(4), hjust = .5,
                                color = '#2b4861', face = 'bold'),
   plot.caption = element_text(family = 'roboto', size = rel(3),
                               color = 'grey70', face = 'italic'),
   axis.text = element_text(size = rel(3)),
   axis.title = element_text(size = rel(3)),
   panel.grid.minor = element_blank(),
   panel.grid.major = element_blank(),
   axis.ticks = element_blank(),
   plot.background = element_rect(fill  = 'white'),
   panel.background = element_rect(fill  = 'white', color = 'black')) -> w39
w39
  
  


# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-09-26", paste0("20230926", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )

ggsave(here::here('2023/w39_TedLasso/w39_plot.png'))
