
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, broom,
  showtext, ggtext, glue,
  patchwork,
  sp, sf, ggthemes, #rgeos, rgdal, 
  geojsonio,
  RColorBrewer)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-07")
df_raw <- tuesdata$house
df_raw |> head() |> view()
df_raw |> filter(state ==  'ALABAMA') |> view()


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""
library(viridis)
map_colors <- rev(magma(8))[c(-1,-8)]

# Data wrangling ----------------------------------------------------------

#totalvotes
tvs <- df_raw |> 
  select(year, state, state_po, party, totalvotes) |> 
  group_by(year, state, state_po, party) |> 
  summarise(total = sum(totalvotes), .groups = 'drop') |> 
  replace_na(list(party = 'UNAFFILIATED'))

abbrev <- tvs |> count(state, state_po) |> 
  select(1:2) |> 
  mutate(state = str_to_title(state))



# map data: geojson
geo_data <- "2023_drafts/rdata/us_states_hexgrid.geojson"
geoj <- geojson_read(geo_data, what = "sp")
geoj@data <- 
  geoj@data |> 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

gsp <- 
  geoj |> tidy()           # broom tidy no longer supported
names <- 
  geoj$google_name |> 
  tibble() |> 
  rename(state = 1) |> 
  mutate(state = str_to_title(state))
ctrs <- 
  gdf |> 
  group_by(id) |> 
  summarise(
    across(
      .cols = c(long,lat),
      .fns  = mean)) |> 
  bind_cols(names) |> 
  inner_join(abbrev, by  = join_by(state)) |> 
  rename(abbr = state_po)


## SF map data
gsf <- geoj |> sf::st_as_sf()
gsf_seleted <- gsf |> select(name = google_name, code = iso3166_2)
gsf_joined <- gsf_seleted |> 
  inner_join(tvs |> rename(code = state_po), 
             by = join_by(code)) |> 
  mutate(tot_100k = total / 100000)

c(seq(5,10), Inf)
hist(gsf_joined$tot_100k)
summary(gsf_joined$tot_100k)
gsf_joined$bin <- cut(
  gsf_joined$tot_100k , 
  breaks=c(0, 10, 50, 100, 150, 200, Inf), 
  labels=c("<1M","1-5M", "5-10M", "10-15M", "15-20M", "20M+"), 
  include.lowest = TRUE )
gsf_joined |> count(bin)
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

# hex map
# hex data link:
# https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map

plot(geoj)        # sp::plot()

  
# sp object
gdf |> 
  ggplot() +
  geom_polygon(
    aes(
      x = long, 
      y = lat, 
      group = group), 
    fill = "skyblue", 
    color = "white") +
  theme_void() +
  coord_map()



gsf |> st_geometry() |> plot()
gsf_joined |> 
  st_geometry() |> 
  plot()



state_code <- 
  gsf_joined |> 
  count(code)
gsf_joined |> head() |> view()

# continuous scale
gsf_joined |> 
  filter(year == 2000) |> 
  ggplot() +
  geom_sf(aes(fill = total)) +
  geom_sf_text(
    aes(label = code), 
    data = state_code |> filter(code != 'DC'))

gsf_joined |> 
  filter(year %in% c(1976, 1982, 1992, 2002, 2012, 2022)) |> 
  ggplot(aes(fill = total)) + 
  geom_sf() +
  facet_wrap(~year) +
  theme_map() +
  theme(legend.position = 'bottom')

# discrete scale
tvs |> count(party, sort = TRUE)
gsf_joined |> 
  mutate(party = case_when(
    str_detect(party, 'DEMOCRAT') ~ 'DEMOCRAT',
    str_detect(party, 'REPUBLICAN') ~ 'REPUBLICAN',
    TRUE ~ 'OTHER')) |> 
  filter(year == 2022) |> 
  ggplot() +
  geom_sf(aes(fill = bin)) +
  geom_sf_text(
    aes(label = code), 
    data = state_code |> filter(code != 'DC')) +
  facet_wrap(~party)

gsf_joined |> 
  filter(year %in% c(1976, 1982, 1992, 2002, 2012, 2022)) |> 
  ggplot(aes(fill = bin)) + 
  geom_sf() +
  facet_wrap(~year) +
  theme_map() +
  theme(legend.position = 'bottom')

# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-11-07", paste0("20231107", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
