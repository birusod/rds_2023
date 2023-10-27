
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, 
  showtext, patchwork, ggtext, glue,
  fmsb, 
  tayloRswift, taylor)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-17")
albums_songs <- tuesdata$taylor_album_songs
all_songs <- tuesdata$taylor_all_songs
albums <- tuesdata$taylor_albums


# Load fonts --------------------------------------------------------------
font_add_google("Lobster Two", "lobstertwo")
font_add_google("Roboto", "roboto")
roboto <- 'roboto'

font_add_google(name = "IM Fell DW Pica", family = "IM Fell DW Pica")
pica <- "IM Fell DW Pica"

font_add_google(name = "Lato", family = "Lato")
lato <- "Lato"
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col1 <- 'black'
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------

# Albums: filtering  out 'Taylor version albums' and null user score
albums |> 
  filter(!is.na(user_score)) |> 
  mutate(album_name2  = str_remove_all(album_name," \\(Taylor's Version\\)")) |> 
  mutate(album_name2  = fct_reorder(album_name2, user_score))


alb_filtered <- albums |> 
  filter(!is.na(user_score),
         !str_detect(album_name, 'Version')) |> 
  mutate(album_name = case_when(
    album_name == 'folklore' ~ 'Folklore',
    album_name == 'evermore' ~ 'Evermore',
    album_name == 'reputation' ~ 'Reputation',
    TRUE ~ album_name)) |> 
  mutate(album_name  = fct_reorder(album_name, user_score))


rdf <- 
  all_songs |> 
  filter(!is.na(album_name),
         !str_detect(album_name, 'Version')) |> 
  mutate(album_name = case_when(
    album_name == 'folklore' ~ 'Folklore',
    album_name == 'evermore' ~ 'Evermore',
    album_name == 'reputation' ~ 'Reputation',
    album_name == 'The Taylor Swift Holiday Collection' ~ 'Holiday',
    TRUE ~ album_name)) |> 
  select(album_name, danceability, energy, loudness, 
         speechiness:tempo) |>
  drop_na() |> 
  group_by(album_name) |> 
  summarise(across(.cols = c(danceability:tempo),
                   .fns = mean)) |> 
  right_join(alb_filtered |> select(album_name, user_score),
             by  = join_by(album_name)) |> 
  rename(album = album_name)
  
  




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
  "**Data**: <br>" #social
)


# Plot --------------------------------------------------------------------

# albums: Bar plot
alb_filtered |> 
  ggplot(aes(user_score, album_name, fill = album_name)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label= album_name, x = 0.2),
    family = pica,
    position = position_dodge(width = .4), 
    hjust = 0, 
    size = 6, 
    color = '#FFFAFA') +
  scale_x_continuous(
    breaks = c(0, 2, 4, 6, 8),
    expand = c(0,0), limits = c(0, 11)) +
  #scale_fill_albums() +
  scale_fill_taylor() +
  labs(x = '',
       y = '',
       title = "TAYLOR SWIFT ALBUMS",
       subtitle = 'Sorted By User Score') +
  theme_minimal(
    base_size = 15,
    base_family = pica
  ) +
  theme(
    plot.background = element_rect(fill = bg_col1, colour = bg_col1),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = .1, color = 'grey50'),
    plot.title = element_text(face = 'bold', color = '#b8396b'),
    plot.subtitle = element_text(color = '#813C6F'),
    axis.title = element_text(color = '#b8396b'),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = '#b8396b', size = rel(1.5)),
    axis.ticks.y = element_blank()
  )

# albums: circular barplot 

rdf2 <- rdf |> 
  select(-user_score) |> 
  pivot_longer(-album, names_to = 'measure', values_to = 'values') |> 
  group_by(measure) |> 
  mutate(values = rescale(values)) 

okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#6C5B7B", "#F8B195")

rdf2 |> 
  #filter(album  == 'Lover') |> 
  ggplot() +
  geom_hline(aes(yintercept = y),  
             data.frame(y = c(0:4) * .25), color = "lightgrey") + 
  geom_col(aes(x = reorder(measure, values),
               y = values, fill = measure,
               )) +
  facet_wrap(~album) +
  geom_segment( aes( 
    x = reorder(measure, values), y = 0,
    xend = reorder(measure, values), yend = 1),
    linetype = "dashed", color = "gray12" ) + 
  coord_polar() +
  annotate(x = 9.7, y = .25, label = "1/4", geom = "text", color = "gray12") +
  annotate(x = 9.7, y = .5, label = "2/4", geom = "text", color = "gray12") +
  annotate(x = 9.7, y = .75, label = "3/4", geom = "text",  color = "gray12") +
  scale_y_continuous(
    limits = c(-.25, 1),
    expand = c(0, 0),
    breaks = c(0, .25, .50, .75)) + 
  scale_fill_manual(values = okabe) +
  guides(
    fill = guide_legend(title = 'Album\nCharacteristics',
                 title.position = 'top',
                 ncol = 2,
                 keywidth = .5, keyheight = .5)) +
  labs(
    x = '', y = '',
    title = "Taylor Swift Albums",
    subtitle = paste(
      "This Visualisation shows Taylor Swift Albums: ",
      "Characteristics scores are rescaled to allow",
      "Albums comparison across these 9 characteristices.",
      sep = ' '),
    caption = 'Tidytuesday | Week-42') +
  theme(
    legend.position = c(.8, .15),
    legend.title = element_text(hjust = .5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 8, color = 'blue', margin = margin()),
    strip.background = element_rect(fill = 'white'),
    panel.spacing = unit(1, 'pt')
  )



# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-10-17", paste0("20231017", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
