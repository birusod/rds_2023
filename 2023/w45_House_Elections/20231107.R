
# 1. Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, broom,
  showtext, ggtext, glue,
  patchwork, ggalt, ggrepel,
  sp, sf, ggthemes, #rgeos, rgdal, 
  geojsonio,
  RColorBrewer)


# 2. Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-07")
df_raw <- tuesdata$house
df_raw |> head() |> view()
#df_raw |> filter(state ==  'ALABAMA') |> view()


# 3. Load fonts and colors   ---------------------------------------
# 3.1 fonts and text ---------------------------------------
font_add_google("Roboto", "roboto")
showtext_auto()


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

# 3.2 Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""
library(viridis)
map_colors <- rev(magma(8))[c(-1,-8)]

library(MetBrewer)
met_col1 <- MetBrewer::met.brewer('Cassatt2')
mcl <- c("#2d223c", "#574571", "#90719f", "#b695bc", "#dec5da", "#c1d1aa", "#7fa074", "#466c4b", "#2c4b27", "#0e2810")
met_pal1 <- met_col1[c(1, 3, 5, 6, 7, 9)]

hiro <- c("#e76254", "#ef8a47", "#f7aa58", "#ffd06f", "#ffe6b7", 
          "#aadce0", "#72bcd5", "#528fad", "#376795", "#1e466e")

met_pal <- c('#a40000', '#ef8a47', '#ffe6b7', 
             '#aadce0', '#72bcd5', '#2e77ab','#16317d')


# 4. Data wrangling ----------------------------------------------------------

# 4.1 totalvotes tsv  -----------------
tvs <- df_raw |> 
  select(year, state, state_po, party, candidatevotes) |> 
  group_by(year, state, state_po, party) |> 
  summarise(total = sum(candidatevotes), .groups = 'drop') |> 
  replace_na(list(party = 'UNAFFILIATED'))

abbrev <- tvs |> count(state, state_po) |> 
  select(1:2) |> 
  mutate(state = str_to_title(state))



# 4.2 map data: geojson ---------
geo_data <- "2023_drafts/rdata/us_states_hexgrid.geojson"
geoj <- geojson_read(geo_data, what = "sp")
geoj@data <- 
  geoj@data |> 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# gsp <- geoj |> tidy()           # broom tidy no longer supported
gdf <- tidy(geoj)

# 4.3 using sf package --------------

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


# 4.4 SF map data (gsf_joined) -------------------
gsf <- geoj |> sf::st_as_sf()
gsf_seleted <- gsf |> select(name = google_name, code = iso3166_2)

c(seq(5,10), Inf)
hist(tvs$total)


gsf_joined <- gsf_seleted |> 
  inner_join(tvs |> rename(code = state_po), by = join_by(code)) |> 
  mutate(
    tot_100k = total / 100000,
    bin = cut(
      tot_100k,
      breaks = c(0, 10, 50, 100, 200, Inf),
      labels = c("<1", "10", "15", "20", "20>"),
      include.lowest = TRUE
    ))

state_code <- 
  gsf_joined |> 
  count(code, bin)
#gsf_joined |> head() |> view()



gsf_joined |>
  ggplot(aes(bin)) +
  geom_bar()



# 4.5 Dem vote  share in 1976 / 2022 (dem_share_year) -------------------

vote_swg <- df_raw |> 
  filter(year %in% c(1976, 2022),
         party %in% c('DEMOCRAT', 'REPUBLICAN')) |> 
  select(year, code = state_po, party, candidatevotes) |> 
  group_by(year, code, party) |> 
  summarise(total = sum(candidatevotes), .groups = 'drop') |> 
  pivot_wider(names_from = party, values_from = total) |> 
  mutate(DEMOCRAT = replace_na(DEMOCRAT, 0),
         REPUBLICAN = replace_na(REPUBLICAN, 0)) |> 
  mutate(DEM = round(DEMOCRAT / (DEMOCRAT + REPUBLICAN) *  100, digits = 0),
         REP = round(REPUBLICAN / (DEMOCRAT + REPUBLICAN) *  100, digits = 0))

dem_share_year <- 
  vote_swg |> 
  select(year, code, DEM) |> 
  pivot_wider(id_cols = code, names_from = year, values_from = DEM) |> 
  mutate(swing = `2022` - `1976`) |> 
  mutate(swing_rescaled = rescale(swing, to = c(-5, 5)))

dsy_joined_cont <- gsf_seleted |> 
  inner_join(dem_share_year, by = join_by(code))

dsy_joined_cont |> filter(code == 'ND')

summary(dsy_joined_cont$swing_rescaled)
dsy_joined_cont |> filter(swing_rescaled >= 3)
s10 <- sample(10)
cut(s10, breaks = c(1, 2, 3, 4))
    
    
dsy_joined_disc <- gsf_seleted |> 
  inner_join(dem_share_year, by = join_by(code)) |> 
  mutate(
    sr_bin = cut(
      swing_rescaled,
      breaks = c(-5, -3, -1, 0, 1, 3, 4, Inf),
      labels = c("-4", "-3", "-1", "0", "+1", "+3", "+4"),
      include.lowest = TRUE
    )) 

state_code_swing <-  dsy_joined |> count(code, swing)
state_code_bins <-  dsy_joined_disc |> count(code, sr_bin)
state_code_bins |> count(sr_bin)

# 5. Plots --------------------------------------------------------------------

# 5.1 hex map
# hex data link:
# https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map

plot(geoj)        # sp::plot()

  
# 5.1 sp object  -------------
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
  coord_map() +
  theme(plot.title.position = 'plot')


# 5.2 sf object ------------
gsf |> st_geometry() |> plot()
gsf_joined |> 
  st_geometry() |> 
  plot()



# 5.3 continuous scale -----------
gsf_joined |> 
  filter(year == 2000) |> 
  ggplot() +
  geom_sf(aes(fill = total)) +
  geom_sf_text(
    aes(label = code), 
    data = state_code |> filter(code != 'DC')) +
  scale_fill_viridis(option = 'A')

gsf_joined |> 
  filter(year %in% c(1976, 1982, 1992, 2002, 2012, 2022)) |> 
  ggplot(aes(fill = total)) + 
  geom_sf() +
  facet_wrap(~year) +
  theme_map() +
  theme(legend.position = 'bottom')

# 5.4 discrete scale -----------------------

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

gg <- gsf_joined |> 
  filter(year %in% c(1976, 1982, 1992, 2002, 2012, 2022),
         party == 'DEMOCRAT') |> # REPUBLICAN  /  DEMOCRAT
  ggplot(aes(fill = bin)) + 
  geom_sf() +
  geom_sf_text(aes(label = code), 
               color = "white", size = 2, alpha = 0.6,
               data = state_code |> filter(code != 'DC')) +
  facet_wrap(~year) +
  theme_map()
  #theme(legend.position = 'bottom') +
  
gg + scale_fill_manual(
  values = map_colors,
  name = NULL,
  guide = guide_legend(
    keyheight = unit(4, units = "mm"),
    keywidth = unit(8, units = "mm"),
    label.position = "top",
    nrow = 1
  )
) +
  labs(
    title = "TOTAL VOTES BY STATE",
    subtitle = "DEMOCRAT PARTY", # REPUBLICAN  /  DEMOCRAT
    caption = "DC not labelled"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.title.position = 'plot',
    plot.background = element_rect(fill = "black", color = NA), 
    plot.title = element_text(
      size = 22, color = "#178a94", 
      family = 'roboto', face = 'bold',
      margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(
      size = 18, color = "#F765B8",
      family = 'roboto', face = 'bold.italic'),
    plot.caption = element_text(color = 'grey80', 
                                size = 8, face = 'italic'),
    panel.background = element_rect(fill = "black", color = NA), 
    legend.background = element_blank(),
    legend.text = element_text(color = '#27fdf5', size = 10),
    legend.position = c(0.75, 1.1),
    strip.background = element_blank(),
    strip.text = element_text(
      color = '#ee7b06', size = 15,
      face = 'bold'),
  )  -> gg_dem


# 5.5 comparison between party votes by state -------------

vote_swg |> 
  select(year, code, DEM , REP) |> 
  pivot_longer(-c(year, code), names_to = 'party', values_to = 'votes') |> 
  filter(year == 1976) |> 
  ggplot(aes(votes, code, fill = party)) +
  geom_col()


dsy_joined_cont |> 
  ggplot(aes(fill = swing_rescaled)) +  # swing_rescaled
  geom_sf() +
  geom_sf_text(aes(label = code), 
               color = "white", size = 2, alpha = 0.6,
               data = state_code_swing_resc |> 
                 filter(code != 'DC') # state_code_swing_resc
               ) + 
  theme_map() +
  scale_fill_viridis_c(option   = 'A')
  
gd <- dsy_joined_disc |> 
  ggplot(aes(fill = sr_bin)) + 
  geom_sf() +
  geom_sf_text(aes(label = code), 
               color = "black", size = 4, alpha = 0.9,
               data = state_code_bins |> 
                 filter(code != 'DC')) + 
  theme_map() 

gd +
  scale_fill_manual(
    values = met_pal,
    name = NULL,
    guide = guide_legend(
      keyheight = unit(4, units = "mm"),
      keywidth = unit(8, units = "mm"),
      label.position = "top",
      nrow = 1)) +
  labs(
    title = "SWING VOTES BY STATE",
    subtitle = "REF: DEMOCRAT PARTY", # REPUBLICAN  /  DEMOCRAT
    caption = "DC not labelled"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.title.position = 'plot',
    plot.background = element_rect(fill = "black", color = NA), 
    plot.title = element_text(
      size = 22, color = "#178a94", 
      family = 'roboto', face = 'bold',
      margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(
      size = 14, color = "#208cc0",
      family = 'roboto', face = 'bold.italic'),
    plot.caption = element_text(color = 'white', 
                                size = 8, face = 'italic'),
    panel.background = element_rect(fill = "grey80", color = NA), 
    legend.background = element_blank(),
    legend.text = element_text(color = '#27fdf5', size = 10),
    legend.position = c(0.6, 1),
    strip.background = element_blank(),
    legend.key = element_blank(),
    strip.text = element_text(
      color = '#ee7b06', size = 15,
      face = 'bold'),
  )


# swing votes barplot / lollipop

dsy_joined_cont |> 
  mutate(code = fct_reorder(code, swing)) |> 
  ggplot(aes(swing, code)) +
  geom_col()  +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-50, 50))

# Lollipop 
djc_lollipop <-  dsy_joined_cont |> 
  mutate(name = fct_reorder(name, swing),
         group = case_when(
             swing > 0 ~ 'Pos',
             swing < 0 ~ 'Neg',
             TRUE ~ 'Neu') |> fct_relevel('Pos', 'Neu', 'Neg'),
         lab = if_else(swing <= 0, 0, 1),
         nudge = if_else(swing > 0, -1, 2)
         )

ggl <- djc_lollipop |>
  ggplot(aes(swing, name, color = group))  +
  geom_text(aes(y = name, x = 0, label = name), 
            hjust=djc_lollipop$lab,
            nudge_x = djc_lollipop$nudge,
            show.legend = FALSE) +
  geom_lollipop(
    point.size = 4, 
    horizontal = TRUE,
    show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  #geom_hline() +
  scale_color_manual(values = c('#4585b7', 'gray70', '#b83326')) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(-50, 50))  +
  labs(x = NULL, y = NULL) 

ggl +
  labs(
    title = "SWING VOTES BY STATE",
    subtitle = "REF: DEMOCRAT PARTY", # REPUBLICAN  /  DEMOCRAT
    caption = "DC not labelled"
  ) +
  theme(
    axis.ticks.y = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    text = element_text(color = "#22211d"),
    plot.title.position = 'plot',
    #plot.background = element_rect(fill = "black", color = NA), 
    plot.title = element_text(
      size = 22, color = "#178a94", 
      family = 'roboto', face = 'bold',
      margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(
      size = 14, color = "white",
      family = 'roboto', face = 'bold.italic'),
    plot.caption = element_text(color = 'white', size = 8, face = 'italic'),
    #panel.grid = element_line(color = 'black'),
    axis.text.x = element_text(color = 'white', size = rel(1.2))
  )
    

# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-11-07", paste0("20231107", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )


gg_dem # democrat party
gg_rep # republican party
