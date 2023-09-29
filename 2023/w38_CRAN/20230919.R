
# Load packages -----------------------------------------------------------

pacman::p_load(here, fs,
  tidytuesdayR, tidyverse, janitor, scales, ggalt,
  showtext, patchwork, ggtext, glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-09-19")
tuesdata

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------

pkgs <- tuesdata$cran_20230905 |> 
  #filter(Package == 'MASS') |> select(Package, Date, `Date/Publication`)
  select(PubDate = `Date/Publication`, Package, Author) |> 
  clean_names() |> 
  mutate(author = str_remove_all(author, "\\[.*\\]"),
         author = str_remove_all(author, "\\(.*\\)")) |> 
  mutate(pub_date = case_when(
    is.na(pub_date) ~ '2023-07-13 07:32:21 UTC',
    TRUE ~ pub_date)) |> 
  mutate(pub_date = ymd_hms(pub_date),
         date = make_date(year(pub_date), month(pub_date), wday(pub_date)),
         year = year(date),
         month  = month(date, label = TRUE))

authors <- pkgs |> 
  select(package, author)  |> 
  separate_rows(author, sep = ' ,\n  ') |>
  mutate(author = str_trim(author))



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

df_plot <- authors |> 
  count(author, sort = TRUE) |> 
  mutate(author = fct_reorder(author, n)) |> 
  slice_head(n = 30) 
df_plot |> 
  ggplot(aes(author, n)) +
  geom_lollipop(point.size = 4, 
                point.colour = '#c95352',
                color = 'grey', size = 2) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), limits = c(0, 180)) +
  labs(x = '', y = '', 
       title = 'MOST PROLIFIC AUTHORS LISTED ON CRAN',
       subtitle = 'Number of times cited as R package author',
       caption = 'TidyTuesday | Week-39') +
  theme(
    plot.title = element_text(family = 'roboto', size = 30, 
                              color = '#c95352', face = 'bold'),
    plot.subtitle = element_text(family = 'roboto', size = 20, 
                              color = '#cc7237', face = 'bold'),
    plot.caption = element_text(family = 'roboto', size = 16, 
                                 color = 'grey70', face = 'italic'),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      linetype = 'dotted', linewidth = .1, color = 'black'),
    axis.text = element_text(face = 'bold', size = 16),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill  = 'white'),
    panel.background = element_rect(fill  = 'white')
  ) -> w38


# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-09-19", paste0("20230919", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
#ggsave(here('2023/w38_CRAN/week39_cran.png'))


library(camcorder)
font_add_google("Roboto", "roboto")
showtext_auto()
showtext_auto(showtext_opts(dpi = 300))

pdir <- '/Users/birusod/Documents/DataScienceDocs/GitProjects/R4DS/rds_2023/2023/w38_CRAN'

w38

gg_record(
  dir = file.path(pdir),
  device = "png",
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)

w38

record_polaroid()
gg_stop_recording()

#ggsave(paste0(pdir, '/final_plot_w38.png'))

gg_playback(
  name = file.path(pdir, "w38_gif.gif"),
  first_image_duration = 5,
  last_image_duration = 15,
  frame_duration = .4,
  image_resize = 800
)
