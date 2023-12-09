
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue, 
  africamonitor, countrycode, ggimage, ggpath,
  GGally, ggalt, ggrepel, ggbump, cowplot)

install.packages("ggflags", repos = c(
  "https://jimjam-slam.r-universe.dev",
  "https://cloud.r-project.org"))
library(ggflags)

# Loading data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-12-05")

tuesdata$life_expectancy |> head(3)
tuesdata$life_expectancy_different_ages |> head(3)
tuesdata$life_expectancy_female_male |> head(3)

# Loading fonts ------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Fira code", "firac")
font_add_google("Fira sans", "firas")
showtext_auto()


# Defining colors ---------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""
pretty_colors <- c("#0f4c5c", "#5f0f40","#0b8199","#9a031e","#b32520","#ffca3a", "#fb8b24")

library(MetBrewer)
met.brewer(name="VanGogh1", n=7, type="discrete")
met.brewer(name="VanGogh1", n=10, type="continuous")
mbcols <- met.brewer(name="OKeeffe1", n=30, type="continuous")[23:30]
# Wrangling Data ----------------------------------------------------------
dd <- tuesdata$life_expectancy
range(dd$Year)

## Joining all data ------
df <- tuesdata$life_expectancy |> 
  left_join(
    tuesdata$life_expectancy_different_ages,
    by = join_by(Entity, Code, Year)) |> 
  left_join(
    tuesdata$life_expectancy_female_male,
    by = join_by(Entity, Code, Year)) |> 
  rename(country = 1,
         code = 2,
         year = 3,
         lifeexp = 4) |> 
  mutate(decade = year %/% 10 * 10,
         country  = case_when(
           country == 'Central African Republic' ~ 'CAR',
           country == 'Democratic Republic of Congo' ~ 'DRC',
           country == 'Equatorial Guinea' ~ 'Equ Guinea',
           country == 'Sao Tome and Principe' ~ 'Sao Tome',
           country == 'Western Sahara' ~ 'West Sahara',
           country == 'Sao Tome and Principe' ~ 'Sao Tome',
           country == 'Western Sahara' ~ 'West Sahara',
           TRUE ~ country
         ))

df_long <- df |>
  pivot_longer(-c(country, code,year),
               names_to = 'cat',
               values_to = 'lifeexp')

range(df$year)
df |> 
  select(country) |> 
  filter(!duplicated(country)) |> 
  view()

##  data: 

### African countries
# am_countries |> head() |> view()
african_countries <- 
  am_countries |> 
  select(name = Country, name2 =  Country_ISO, code  = ISO3, 
         code2 = ISO2, region = Region, subregion  = Region_Detailed)


## Subsetting african data ----

afr <- african_countries |>
  left_join(df, by = join_by(code))



## Timeframe data: -------
#1950 vs 2021 (years with complete rows)
tfd <- afr |> 
  filter(year %in% c(1950, 2021)) |> 
  select(country, year, lifeexp) 

## Before-After data: -------
bad <- afr |> 
  filter(year %in% c(1950, 2021)) |> 
  select(country, year, lifeexp) |> 
  mutate(year = factor(year))



##  Selected  countries:  -------
# df |>  filter(team %in% (df |> 
#                    distinct(team, mean_rank) |> 
#                    top_n(10, -mean_rank) |> 
#                    pull(team)))

#afr |>  distinct(name, code2) |> view()
mcs <- c('Mali', 'Niger', 'Burkina Faso', 'Sierra Leone', 'Guinea', 'Liberia')
df6 <- afr |> 
  filter(name  %in% mcs, year %in% c(1950, 1960, 1980, 2000, 2020)) |> 
  select(name, year, lifeexp) |> 
  group_by(year) |> 
  mutate(rank = rank(-lifeexp, ties.method = "random")) |> 
  ungroup()


ccode <- 
  countrycode(df6$name |> unique() |> sort(),
            origin = "country.name",
            destination = "genc2c") |> 
  tolower() |> 
  set_names(df6$name |> unique() |> sort())
ccode['Mali']

df6 <- df6 |> 
  left_join(afr |> distinct(name, code2), by  = join_by('name')) |> 
  mutate(code = str_to_lower(code2))


## flags ----
flags <- c('Mali', 'Niger', 'Senegal', 'Chad', 'Guinea', 'Liberia', 'Ghana')

dff <- afr |> 
  filter(name  %in% flags, year == 2021) |> 
  select(name, year, lifeexp)

# Define texts & annotations --------------------------------------------

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


## Barh plot -----
afr |> 
  filter(year == max(year)) |> 
  mutate(country = fct_reorder(country, lifeexp)) |> 
  ggplot(aes(lifeexp, country)) +
  geom_col(width = .6) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 80)) +
  labs(y = NULL, x = NULL,
       title = 'African Countries',
       subtitle = 'Life Expectenccy in 2021') +
  theme_light() +
  theme(
    axis.ticks.y = element_blank()
  )




## Dumbbell plot  -----
# https://r-charts.com/distribution/dumbbell-plot-ggplot2/#data


# long data
country_order <- tfd |> 
  filter(year == 1950) |> 
  arrange(lifeexp) |> 
  pull(country)

tfd |> 
  mutate(country  = factor(country, levels = country_order),
         year = factor(year)) |> 
  ggplot(aes(x = lifeexp, y = country)) +
  geom_line() +
  geom_point(aes(color = year), size = 3) +
  scale_color_brewer(palette = "Set1", direction = -1) +
  labs(y = NULL, x = NULL, color = NULL,
       title = 'African Countries',
       subtitle = 'Life Expectenccy in 1950 and 2021') +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = 'grey95'),
    axis.text.y = element_text(color = 'black', size = 8),
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust = .5),
    legend.position = "right")  

# wide data
tfd |> 
  pivot_wider(names_from = year, values_from = lifeexp,
              names_prefix = 'Y') |> 
  mutate(country = fct_reorder(country, Y2021),
         gain = round(Y2021 - Y1950, 0)) |> 
  ggplot() +
  geom_segment(aes(
    x = Y1950, xend = Y2021,
    y = country, yend = country),
    color = 'grey') +
  geom_point(aes(Y1950, country), color = 'darkorange', size = 2.5) +
  geom_point(aes(Y2021, country), color = 'forestgreen', size = 2.5) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(20, 80),
                     breaks = c(30, 50, 70)) +
  geom_text(aes(x = Y2021, y = country, label = gain), 
            size = 2, nudge_x = 1.5, nudge_y = 0) +
  labs(y = NULL, x = NULL,
       title = 'Life Expectenccy In African',
       subtitle = 'Years gained betwen 1950 and 2021') +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = 'grey95'),
    axis.text.y = element_text(color = 'black', size = 8),
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(hjust = .5)
  )

# ggalt
tfd |> 
  pivot_wider(names_from = year, 
              values_from = lifeexp,
              names_prefix = 'Y') |> 
  mutate(country = fct_reorder(country, Y2021)) |> 
  ggplot(aes(y = country, x = Y1950, xend = Y2021)) +
  geom_dumbbell(color = "darkgray", 
                size = 1,           
                dot_guide = FALSE,   # guide from origin to X or not
                size_x = 3,          
                size_xend = 3,       
                colour_x = "#F69541",    
                colour_xend = "#699DC6") +
  geom_text(aes(x = Y2021, 
                y = country, 
                label = paste0('+', round(Y2021 - Y1950, 0))), 
            size = 2, nudge_x = 1.5, nudge_y = 0) +
  labs(y = NULL, x = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank())
  

## Parallel plot: ------  

### ggplot2  
afr |> select(subregion) |> distinct()
afr |> 
  select(country, year, lifeexp, subregion) |> 
  filter(year %in% c(1960, 1980, 2000, 2021),
         subregion == 'Western Africa') |> 
  ggplot(aes(
    x = year,
    y = lifeexp,
    group = country,
    color = country)) +
  geom_line(size = 2, alpha = 0.5) +
  geom_point(size = 3)


### ggally: ggparcoord

afr |> 
  filter(subregion == 'Middle Africa',
         year >= 1950,
         year <= 2020) |> 
  select(country, lifeexp, decade) |> 
  group_by(country, decade) |> 
  summarise(lifeexp =  mean(lifeexp), .groups = 'drop') |> 
  pivot_wider(names_from = decade, values_from = lifeexp) |> 
  ggparcoord(
    columns = 2:ncol(.), 
    groupColumn = "country",
    scale = "center", scaleSummary = "mean")  +
  theme_light() +
  labs(title = 'Life Exptency in Africa',
       subtitle = 'Comparing middle african countries')


afr_pp <- afr |> 
  filter(subregion == 'Middle Africa',
         decade %in% c(1950, 2020)) |> 
  select(country, lifeexp, decade) |> 
  group_by(country, decade) |> 
  summarise(lifeexp =  mean(lifeexp), .groups = 'drop') 

afr_pp |> filter(decade == 2020)
afr_pp |> 
  ggplot(aes(
    x = decade, y = lifeexp,
    group = country, color = country)) +
  geom_line(size = 2, alpha = 0.5, show.legend = FALSE) +
  geom_point(size = 3, show.legend = FALSE) +
  geom_text_repel(
    data = afr_pp |> filter(decade == 2020),
    aes(x = 2020, y = lifeexp,
        label = paste0(country, ': ', round(lifeexp, 1))), 
    size = 2, nudge_x = 3, nudge_y = 0, hjust = 0, 
    min.segment.length = 1, show.legend = FALSE) +
  scale_x_continuous(breaks = c(1950, 2020),
                     limits = c(1949, 2050))  +
  scale_color_manual(values = ggsci::pal_jco()(10)) +
  theme_minimal() +
  theme(
    text = element_text(color = 'white'),
    axis.text = element_text(color = 'white'),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(
      linetype = 'dashed', color = 'grey40', linewidth = .1),
    panel.background = element_rect(fill = 'white'),
    plot.background = element_rect(fill = 'black'),
    plot.title = element_text(face = 'bold', size = 18))  +
  labs(y = NULL, x = NULL,
       title = 'Life Exptency in Africa',
       subtitle = 'Comparing middle african countries')



## Bump chart: ggbump  -------
smp_years <- c(1960, 1990, 2020)
afr_bb <- afr |> 
  filter(subregion == 'Middle Africa', year %in% smp_years) |> 
  select(country, year, lifeexp)  |> 
  group_by(year) |> 
  mutate(rank = rank(lifeexp)) |> 
  ungroup()

afr_bb |> filter(year == 1960)

afr_bb |> 
  ggplot(aes(year, rank, color = country)) +
  geom_bump(show.legend = FALSE, linewidth = 2) +
  geom_text_repel(
    data = afr_bb |> filter(year == 2020),
    aes(x = 2021, y = rank,
        label = paste0(country, ': ', round(lifeexp, 1))),
    size = 4, 
    nudge_y = .2, 
    hjust = 1.2, 
    fontface  = 'bold',
    min.segment.length = 2, show.legend = FALSE)

gain_df <- afr_bb |> 
  select(country, year, lifeexp) |> 
  filter(year %in% c(min(year), max(year))) |> 
  pivot_wider(names_from = year, values_from = lifeexp) |> 
  mutate(gain = round(`2020` - `1960`, 0)) |> 
  select(country, gain)

afr_bb2 <- afr_bb |> left_join(gain_df, by = join_by(country)) 
afr_bb2 |> 
  ggplot(aes(year, rank, color = country)) +
  geom_point(size = 7) +
  geom_text(data = afr_bb2  |> filter(year == min(year)),
            aes(x = year - 3, label = country), 
            size = 4, hjust = 1) +
  geom_text(data = afr_bb2  |>  filter(year == max(year)),
            aes(x = year + 3, label = paste0(country, '\nGain: ', gain)), 
            size = 4, hjust = 0) +
  geom_bump(size = 2, smooth = 8) +
  scale_x_continuous(limits = c(1940.6, 2040),
                     breaks = seq(1960, 2020, 30)) +
  labs(y = "RANK (1 = lowest)", x = NULL,
       title = 'Life Exptency In Africa',
       subtitle = 'Comparing Middle African Countries') +
  scale_y_reverse(breaks = seq(1, 10, 1)) +
  scale_color_manual(values = tableau_color_pal('Tableau 10')(10))  +
  theme_minimal_grid(font_size = 14, line_size = 0) + # cowplot
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        text = element_text(color = 'white'),
        axis.text = element_text(color = 'white'),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = 'black'),
        plot.background = element_rect(fill = 'black'),
        plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        plot.subtitle = element_text(size = 16, hjust = .5))



## Selected ccountries ------

# Not working. To be continued....

# df6 |> 
#   ggplot(aes(year, rank, group = name, color = name, fill = name)) +
#   geom_bump(smooth = 10, size = 1.5, lineend = "round") +
#   ggimage::geom_flag(aes(x = 1950, image = code), size = 0.1)
#   geom_text(data = df6 |> filter(year == 2020),
#             aes(label = name),
#             color = "black") +
#   ggimage::geom_flag(data = df6 |> filter(year == 1950),
#                      aes(y = rank, image = code)) +
#   ggimage::geom_flag(data = df6 |> filter(year == 1950), 
#             aes(image = code)) +
#   scale_color_manual(
#     values = pretty_colors) +
#   scale_fill_manual(values = c(wesanderson::wes_palette("GrandBudapest2"), wesanderson::wes_palette("GrandBudapest1"), wesanderson::wes_palette("BottleRocket2"))) +
#   scale_y_reverse(breaks = 1:100) +
#   scale_x_continuous(breaks = df$year |> unique()) +
#   theme_minimal() +
#   theme(legend.position = "none",
#         panel.grid = element_blank(),
#         panel.background = element_rect(fill = "gray60", color = "transparent"),
#         plot.background = element_rect(fill = "gray60"),
#         text = element_text(color = "white")) +
#   labs(x = NULL,
#        y = NULL,
#        title = "Hosting is boosting the number of medals",
#        subtitle = "Number of medals on Summer Olympics 1992 - 2016")
# 
# df6 |>filter(year == 1950)
# 
# df6 |> 
#   group_by(name) |> 
#   top_n(1, lifeexp) |> 
#   ggplot(aes(x = fct_reorder(name, lifeexp), 
#            y = lifeexp, 
#            group = name, 
#            fill = name)) +
#   geom_col() +
#   coord_flip() +
#   geom_text(aes(label = round(lifeexp, digits = 0)), 
#             hjust = 1, 
#             size = 5,
#             position = position_dodge(width = 1),
#             inherit.aes = TRUE) +
#   expand_limits(y = 63) +
#   labs(title = "Title",
#        subtitle = "Subtitle") +
#   theme(legend.position = "none") +
#   scale_fill_manual(values = pretty_colors) +
#   ggimage::geom_flag(aes(y = -4, image = code), size = 0.1) #+ #ggimage::
  #ggflags::geom_flag(aes(y = -4, country = code), size = 0.1)


## Flags plots -----
# https://uxwing.com/chad-flag-round-circle-icon/

flag_path <- "/Users/birusod/Documents/DataScienceDocs/RDocs/ggplot2/flags/"

fcols <- c("#0f4c5c", "#0b8199", "#5f0f40", "#9a031e", 
           "#b32520", "#fb8b24","#ffca3a" )

dff |> 
  mutate(flag = glue(flag_path, '{name}-flag-round-circle-icon.png'),
         name = fct_reorder(name, lifeexp)) |> 
  ggplot(aes(lifeexp, name, fill = name)) +
  geom_col(width = .9, show.legend = FALSE) +
  geom_from_path(aes(lifeexp, name, path = flag), width = 0.12) +
  scale_fill_manual(
    values = mbcols,
    guide = "none") +
  geom_label(
    aes(x = 0, y = name, label = paste0(name, '\n',round(lifeexp, 0), ' years')),
    position = position_dodge(0),
    size = 4, color = 'wheat', hjust = 'left', label.size = 0,
    family = "roboto", fontface = "bold") +
  scale_x_continuous(expand = c(.05, .01),
                     limits = c(0, 80)) +
  labs(title = 'LIFE EXPECTENCY IN AFRICA IN 2021',
       subtitle = 'Selected countries') +
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'black'),
    plot.title = element_text(family = 'roboto', face = 'bold', 
                              color = '#94d2bd', size = 16, hjust = .5),
    plot.subtitle = element_text(family = 'firas', face = 'italic', 
                              color = '#0a9396', size = 16, 
                              hjust = .5)
  )

# Saving Plots and Gifs ------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-12-05", paste0("20231205", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
