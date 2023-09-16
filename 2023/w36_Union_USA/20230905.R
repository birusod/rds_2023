
# Load packages -----------------------------------------------------------
pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, MetBrewer,
  showtext, patchwork, ggtext, glue, 
  maps, mapproj, usmap,
  camcorder)


# Load data ---------------------------------------------------------------
tuesdata <- tt_load("2023-09-05")
# readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
# wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
# states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

tuesdata # viewing

# Load fonts --------------------------------------------------------------
font_add_google("Roboto", "roboto")
font_add_google('Lobster', 'lobster')
font_add_google('Anton', 'anton')
font_add_google('Fira Sans', 'firasans')
font_add_google('Syne Mono', 'syne')

showtext_auto()
showtext_auto(showtext_opts(dpi = 300))


# Define colours ----------------------------------------------------------
# thematic::okabe_ito()
#"#E69F00" "#009E73" "#0072B2" "#CC79A7" "#999999" "#D55E00" "#F0E442" "#56B4E9"


# Data wrangling ----------------------------------------------------------

demographics <- tuesdata$demographics   
wages <- tuesdata$wages
states <- tuesdata$states

# Percent of employed workers who are union members.
union_pct <- states |> 
  filter(year %in% c(1983, 1980, 1990, 2000, 2010, 2020, 2022)) |> 
  select(year, state = state_abbreviation, p_members) |> 
  group_by(year, state) |> 
  summarise(pct = mean(p_members))

union_pct |> head()

# Define text -------------------------------------------------------------

mytitle <- "<span style = 'color:#E69F00;'>Percent Of Employed Workers Who Are</span>
  <i style = 'color:#009E73;'>Union Members</i>
  <span style = 'color:#CC79A7;'>in 2022</span>"

cap <- paste0("*TidyTuesday Data | Week-36*")


# Plot --------------------------------------------------------------------
union_pct$pct |> hist()  # EDA

# Plot-1
union_pct |> 
  filter(year == 2022) |> 
  plot_usmap(data = _, values = 'pct', 
             labels = TRUE, label_color = 'white',
             color = 'grey95', linewidth = .1)  +
  scale_fill_viridis_c(option = 'G',
                       direction = -1,
                       breaks = c(.05, .1, 15, .2, .25),
                       labels = paste0(c(5, 10, 15, 20, 25), '%'),
                       guide = guide_legend(
                         direction = 'horizontal',
                         keyheight = unit(5, units = 'mm')
                       ))  +
  labs(title = mytitle,
       caption = cap) +
  theme(
    plot.title = element_textbox_simple(size = 15, face = 'bold', 
                                        halign = .5, family = 'roboto'),
    plot.caption = element_textbox_simple(halign = .5, face  = 'italic', 
                                          color = 'white', size = 10),
    plot.caption.position = "plot",
    plot.margin = margin(.5,0,0,0, unit = 'cm'),
    
    legend.background = element_blank(),
    legend.position = 'top',
    legend.title = element_blank(),
    legend.justification = 'center',
    legend.text = element_text(face  = 'bold', size = 12, color = 'white')
    ) -> p1

# Plot-2
union_pct |> 
  plot_usmap(data = _, values = 'pct', 
             color = 'grey95', linewidth = .05)  +
  facet_wrap(~year) +
  scale_fill_gradient2(low = munsell::mnsl("5R 5/10"),
                       high = munsell::mnsl("10B 5/8"),
                       mid = munsell::mnsl("5PB 7/8"),
                       midpoint = .25,
                       breaks = c(.1, .2, .3, .4),
                       labels = paste0(c(10, 20, 30, 40), '%'),
                       guide = guide_legend(
                         direction = 'horizontal',
                         keyheight = unit(5, units = 'mm')
                       )) +
  labs(title = 'Percent Of Employed Workers\nWho Are Union Members',
       fill  = 'Percentage') +
  theme(
    #legend.position = 'none',
    legend.position = c(.5, -0.1),
    legend.justification = 'center',
    legend.background = element_blank(),
    legend.text = element_text(color = 'white', face  = 'bold', size = 12),
    legend.title = element_blank(),
    legend.key = element_rect(color = 'black'),
    plot.margin = margin(0.5,0.5,.5,0.5, unit = 'cm'),
    plot.title = element_text(
      hjust = .5, #vjust = 20, 
      #margin = margin(b = 1, unit = 'cm'),
      family = 'roboto', face = 'bold', size = 18, color =  'white'),
    strip.background = element_blank(),
    strip.text = element_text(
      family = 'roboto', face = 'bold', 
      size = 12, color = 'white'),
    plot.background = element_rect(fill = 'black'),
  ) -> p2
p1
p2
# Save gif ----------------------------------------------------------------

ggsave('2023/w36_Union_USA/union_members_2022.png', p1)

pdir <- '/Users/birusod/Documents/DataScienceDocs/GitProjects/R4DS/rds_2023/2023/w36_Union_USA'

gg_record(
  dir = file.path(pdir, "plot1"),
  device = "png",
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)

p1
p2
record_polaroid()
gg_stop_recording()

