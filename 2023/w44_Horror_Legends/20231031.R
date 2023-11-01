
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, 
  showtext, patchwork, ggtext, glue,
  webr, ggpubr)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-31")
ha <- tuesdata$horror_articles
ha |> glimpse()
ha |> head() |> view()
# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Eater", "eater")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""
pal1 <- c('#E26B0F', '#F1A73A', '#59351C', '#9451BB', '#853174',
          '#090303', '#F8731E', '#FFD401', '#B5C802', '#EF0001')
pal2 <- c('#CE2426', '#EE8A34', '#474749', '#3AC561', '#015079', 
          '#017BB4', '#00AFED', '#1B0F0B', '#FE9F08')

# Data wrangling ----------------------------------------------------------

ha |> 
  mutate(rating  = fct_lump_n(rating, 3, other_level = 'mixture')) |> 
  count(rating)

haf <- 
  ha |> 
  mutate(year = year(published),
         month = month(published, abbr = FALSE, label = TRUE),
         day = wday(published, abbr = FALSE, label = TRUE),
         rating  = fct_lump_n(rating, 3, other_level = 'unclear'),
         myear = format(published, "%Y-%b"),
         decade  = year %/% 10 *  10,
         author2 = fct_lump_n(author, 2, other_level = 'Other')) |> 
  select(published, day, month, year, myear, decade, rating, author, author2)


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

#   by year
haf |> 
  ggplot(aes(year)) +
  geom_bar(fill = pal2[3])  +
  labs(x = '', y = '', title = 'HORORS ARTICLES', 
       subtitle = 'From 1997  to 2013') +
  theme_light()
  

haf |> 
  count(year) |> 
  ggplot(aes(year, n)) +
  geom_line()


# by decade
haf |>
  count(decade, rating) |> 
  ggplot(aes(decade, n, fill = rating)) +
  geom_col(position = 'fill')

haf |>
  count(decade, rating) |> 
  PieDonut(aes(decade, count = n))

haf |>
  count(decade, rating) |> 
  PieDonut(aes(decade, rating, count = n))


# by author

haf |> 
  ggplot(aes(fct_rev(fct_infreq(author)))) +
  geom_bar(fill = pal1[4]) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0))  +
  labs(x = '', y = '', title = 'HORORS INVESTIGATORS', 
       subtitle = 'Ranked by popularity') +
  theme_minimal(base_size = 14 ) +
  theme(plot.title.position = 'plot',
        plot.title = element_text(
          face = 'bold', size = rel(2), hjust = .5, 
          color = pal2[4], family = 'eater'),
        plot.subtitle = element_text(
          face = 'bold', size = rel(1.5), hjust = .5,
          color = pal1[3], family = 'ubuntu'),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(
          face = 'bold', family = 'roboto',
          color = pal1[5])
  )

haf |> 
  count(author2, name = 'total') |> 
  ggplot() +
  geom_bar(
    aes(x = '', y = total, fill = author2),
    stat = 'identity', width = 1, fill = NA) +
  geom_bar(
    aes(x = '', y = total, fill = author2),
    stat = 'identity', width = .3) +
  coord_polar(theta = 'y') +
  theme_void() +
  theme(legend.position = 'bottom')

haf |> 
  count(author2, name = 'total') |> 
  mutate(hole = 10,
         pct = round(total / sum(total) * 100,0)) |> 
  ggplot(aes(hole, pct, fill  = author2)) +
  geom_col(show.legend = FALSE) +
  coord_polar(theta = 'y') +
  geom_label(aes(label = paste0(author2, '\n', pct, '%')),
             show.legend = FALSE,
             nudge_x = .2,
             fill = NA) +
  scale_fill_brewer(palette = "Dark2") +
  xlim(c(8, 10 + 0.5)) +
  theme_void() 

haf |> 
  count(author2, name = 'total') |> 
  mutate(
    pct = round(total/sum(total)*100,0),
    lab = paste0(author2, '\n', pct , '%')) |> 
  ggdonutchart('total', 
               label = 'lab', 
               fill = 'author2',
               lab.pos = "in", 
               lab.font = c(5, "black", 'bold'),
               color = "white",
               palette = c("#00AFBB", '#853174', "#FC4E07")) |> 
  ggpar(title = "AUTHORS DISTRIBUTION",
        legend = 'none',
        ticks = FALSE,
        tickslab  = FALSE
  ) +
  theme(plot.title = element_text(
    family = 'roboto', 
    face = 'bold', size = 20,
    color = 'firebrick', hjust = .5))
  


# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-10-31", paste0("20231031", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
