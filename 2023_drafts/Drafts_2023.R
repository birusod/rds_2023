
### Week 36 - Union Members USA ----------------------------------------------
demographics |> colnames()

#  Percent of employed workers who are union members.
demographics |> count(facet)
demographics |> 
  filter(facet %in% c('demographics: male', 'demographics: female')) |> 
  mutate(facet = str_remove_all(facet, 'demographics: ')) |> 
  select(year, p_members, facet) |> 
  ggplot(aes(year, p_members, color = facet)) +
  geom_line()




wages|> colnames()

# Mean wage among union members  vs non-union
wages |> count(facet)

wages |> 
  filter(facet %in% c('demographics: male', 'demographics: female')) |> 
  mutate(facet = str_remove_all(facet, 'demographics: ')) |> 
  select(year, union_wage, nonunion_wage, facet) |> 
  pivot_longer(contains('union'), 
               names_to = 'category', 
               values_to = 'avg_wage') |> 
  ggplot(aes(year, avg_wage, color = category)) +
  geom_line() +
  facet_wrap(~facet)



states|> colnames()
states |> count(sector)

# Employed workers who are union members in thousands.
states |> 
  filter(
    state_abbreviation  %in% c(
      'CA', 'WA', 'NY', 'MA', 'TX', 'SC', 'AR', 'MS')) |> 
  group_by(year, state) |> 
  summarise(members =  sum(members)) |> 
  ggplot(aes(year, members, color = state))  +
  geom_line(linewidth = 2)


# Percent of employed workers who are union members by year, sector, states
states |> 
  filter(
    year %in% c(1983, 1990, 2000, 2010, 2020, 2022),
    state_abbreviation  == 'WA',
    sector %in% c('Private', 'Public')) |>
  mutate(year = as.character(year)) |> 
  ggplot(aes(year, p_members, fill = sector)) +
  geom_col() +
  facet_wrap(~sector) +
  scale_y_continuous(labels = percent)

states |> 
  filter(year  == 2022) |> 
  group_by(state) |> 
  summarise(pct = mean(p_members)) |> 
  mutate(state = fct_reorder(str_to_upper(state), pct)) |> 
  ggplot() +
  geom_point(aes(pct, state))  +
  geom_segment(
    aes(x = 0, xend = pct,y = state, yend = state),
    linewidth = .2, linetype = 'dotted')  +
  labs(title = 'title', x = '', y  = '')  +
  scale_x_continuous(
    expand = c(0,0), 
    labels = percent, 
    limits = c(0, .28)) +
  theme_minimal(base_family = 'roboto') +
  theme(
    axis.text.y = element_text(size = 8, face = 'bold'),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
    )
  

states |> 
  mutate(decade = (year %/% 10) * 10) |> 
  count(decade)

# only 48 + DC
mdat <- map_data('state') |> as_tibble()
mdat |> 
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group), fill = 'grey70', color = 'white')  +
  coord_map()


#usmap
plot_usmap()
dp <- countypov |> select(1, 3:4)
plot_usmap(data = countypov, values = 'pct_pov_2014')
dp |> plot_usmap(data = _, values = 'pct_pov_2014')


states |> colnames()
# Percent of employed workers who are union members.
union_pct <- states |> 
  filter(year %in% c(1983, 1980, 1990, 2000, 2010, 2020, 2022)) |> 
  select(year, state = state_abbreviation, p_members) |> 
  group_by(year, state) |> 
  summarise(pct = mean(p_members))

union_pct$pct |> hist()

colors <-  thematic::okabe_ito(6)
#thematic::okabe_ito() "#E69F00" "#009E73" "#0072B2" "#CC79A7" "#999999" "#D55E00" "#F0E442" "#56B4E9"
mytitle <- glue::glue(
  "<span style = 'color:#E69F00;'>Percent Of Employed Workers Who Are</span>
  <i style = 'color:#009E73;'>Union Members</i>
  <span style = 'color:#CC79A7;'>in 2022</span>"
)
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
  labs(title = mytitle) +
  theme(
    plot.title = element_textbox_simple(size = 12, face = 'bold', family = 'roboto'),
    plot.margin = margin(.3,0,0,0, unit = 'cm'),
    
    legend.position = 'top',
    legend.key.width  = unit(1, "cm"),
    legend.title = element_blank(),
    legend.justification = 'center',
    legend.text = element_text(face  = 'bold', size = 12))
  
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
    plot.margin = margin(0.5,0.5,1.2,0.5, unit = 'cm'),
    plot.title = element_text(
      hjust = .5, #vjust = 20, 
      #margin = margin(b = 1, unit = 'cm'),
      family = 'roboto', face = 'bold', size = 18, color =  'white'),
    strip.background = element_blank(),
    strip.text = element_text(
      family = 'roboto', face = 'bold', 
      size = 12, color = 'white'),
    plot.background = element_rect(fill = 'black'),
  ) 
  



#  munsell
# https://github.com/cwickham/munsell
library(munsell)
my_blue <- "5PB 5/8"
my_blue2 <- "5R 5/1"
p <- plot_mnsl(c(
  lighter(my_blue, 2),      my_blue,   darker(my_blue, 2),
  desaturate(my_blue, 1),   my_blue,   saturate(my_blue, 2),
  rygbp(my_blue, 2),        my_blue,   pbgyr(my_blue, 2)))
p
p2 <- plot_mnsl(c(lighter("5R 5/10"), darker("5R 5/10")))
p2



### Week 37 - The Global Human Day ------------------------------------------
all_countries |> tail(10) |> view()
regions |> head(10) |> view()
ghd |> head(10) |> view()
gea |> head(10) |> view()

# global economic activity
#mytitle <- "<span style = 'color:#E69F00;'>Percent Of Employed Workers Who Are</span>
#  <i style = 'color:#009E73;'>Union Members</i>
#  <span style = 'color:#CC79A7;'>in 2022</span>"

#cap <- paste0("*TidyTuesday Data | Week-36*")


# global human day
ghd |> arrange(hoursPerDay)

ghd |> 
  mutate(Subcategory  = fct_reorder(Subcategory, hoursPerDay)) |>
  ggplot(aes(hoursPerDay, Subcategory)) +
  geom_col()

ghd |> 
  mutate(
    Subcategory  = fct_lump_min(
      Subcategory, min = .5, w = hoursPerDay) |> fct_reorder(hoursPerDay),
    clr = if_else(Subcategory == 'Other', 'no', 'yes')) |> 
  ggplot(aes(hoursPerDay, Subcategory, fill = clr)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(2.5, 5, 7.5),
                     limits = c(0, 9.5)) +
  scale_fill_manual(values = c('grey70', 'steelblue')) +
  labs(y = '', x = '', 
       title = 'Global Human Day',
       subtitle = 'The number of hours per day engaged in each activity averaged across all humans',
       caption  = 'TidyTuesday Data | Week-37')  +
  theme_light(base_family = 'roboto') +
  theme(
    plot.title = element_text(face = 'bold', size = 16),
    plot.subtitle = element_text(face = 'bold', size = 10),
    axis.text = element_text(face = 'bold', size = 10),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )




gea |> arrange(hoursPerDay)

gea |> 
  mutate(
    Subcategory = fct_lump_n(
      Subcategory, 10, w = hoursPerDay) |> fct_reorder(hoursPerDay),
    clr = if_else(Subcategory == 'Other', 'no', 'yes')) |> 
  ggplot(aes(hoursPerDay, Subcategory, fill = clr)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0,0),
                     breaks = c(0.2, 0.4, 0.6),
                     limits = c(0, .75)) +
  scale_fill_manual(values = c('grey70', 'dodgerblue')) +
  labs(y = '', x = 'Hours per day', 
       title = 'Global Economic Activity',
       subtitle = 'Time Devoted to the Global Economy',
       caption  = 'TidyTuesday Data | Week-37') +
  theme_light(base_family = 'roboto') +
  theme(
    plot.title = element_text(face = 'bold', size = 16),
    plot.subtitle = element_text(face = 'bold', size = 14),
    axis.text = element_text(face = 'bold', size = 10),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )



# world map: rnaturalearth, rnaturalearthdata

world_data <- ne_countries(scale = "medium", returnclass = "sf") 
afr_no_data <- world_data |> 
  filter(continent  == "Africa")
afr <- world_data |> 
  filter(continent  == "Africa") |> 
  select(iso_a3, name, name_long, formal_en, formal_fr, 
         economy, income_grp, pop_est, gdp_md)

ggplot(data = world_data) +
  geom_sf(aes(fill = pop_est))  +
  coord_sf(xlim = c(-165, 165), ylim = c(-53, 80))  +
  labs(title = 'World map') +
  theme_map() +
  theme(plot.title = element_text(face = 'bold', hjust = .5))
ggplot(data = afr) +
  geom_sf(aes(fill = pop_est))  +
  coord_sf(xlim = c(-165, 165), ylim = c(-53, 80))  +
  labs(title = 'Africa') +
  theme_map() +
  theme(plot.title = element_text(face = 'bold', hjust = .5))


cdf <- 
  all_countries |> 
  select(country_iso3, Category, Subcategory, hoursPerDayCombined) 
df <- 
  afr |>
  left_join(cdf, by = join_by(iso_a3  == country_iso3))

df |> 
  as_tibble() |> 
  select(Subcategory) |> # check !!!
  unique() |> view()


data()
iso3166 |> head()
countries110 |> as_tibble() |> head()

# social:
afr |> 
  filter(Subcategory == 'Social') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  scale_fill_gradient2(
    low = 'darkorange', mid = 'wheat', high = 'firebrick',
    midpoint = 1.15,
    na.value = "grey70",
    guide = guide_legend(
      title = element_blank(),
      label.position = 'top',
      keywidth = unit(1, 'cm'),
      keyheight = unit(.3, 'cm'))) +
  theme_map() +
  theme(legend.direction = 'horizontal', 
        legend.position = c(.3, .05),
        plot.title = element_text(hjust = .5)) +
  labs(title = 'SOCIAL') -> s_plot

# Religious practice
afr |> 
  filter(Subcategory == 'Religious practice') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  scale_fill_gradient2(
    low = 'magenta', mid = 'azure', high = 'navyblue',
    midpoint = .4,
    guide = guide_legend(
      title = element_blank(),
      label.position = 'top',
      keywidth = unit(1, 'cm'),
      keyheight = unit(.3, 'cm'))) +
  theme_map() +
  theme(legend.direction = 'horizontal', 
        legend.position = c(.3, .05),
        plot.title = element_text(hjust = .5)) +
  labs(title = 'RELIGION') -> r_plot

# Hygiene & grooming  
afr |> 
  filter(Subcategory == 'Hygiene & grooming') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  scale_fill_gradient2(
    low = '#77ccff', high = '#0044ff',
    midpoint = 0.95,
    guide = guide_legend(
      title = element_blank(),
      label.position = 'top',
      keywidth = unit(1, 'cm'),
      keyheight = unit(.3, 'cm'))) +
  theme_map() +
  theme(legend.direction = 'horizontal', 
        legend.position = c(.3, .05),
        plot.title = element_text(hjust = .5)) +
  labs(title = 'HYGIENE/GROOMING')  -> h_plot

# Meals 
'#E76F51''#F4A261''#E9C46A''#2A9D8F''#264653''

afr |> 
  filter(Subcategory == 'Meals') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  scale_fill_gradient2(
    low = '#F4A261' , mid = 'azure', high = '#2A9D8F',
    midpoint = 1.6,
    guide = guide_legend(
      title = element_blank(),
      label.position = 'top',
      keywidth = unit(1, 'cm'),
      keyheight = unit(.3, 'cm'))) +
  theme_map() +
  theme(legend.direction = 'horizontal', 
        legend.position = c(.3, .05),
        plot.title = element_text(hjust = .5)) +
  labs(title = 'MEALS') -> m_plot

afr |> 
  filter(Subcategory %in% c('Social', 'Religious practice', 
                            'Hygiene & grooming', 'Meals')) |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hpd)) +
  facet_wrap(~Subcategory, ncol = 4) +
  scale_fill_gradient2(
    low = '#F4A261' , mid = 'azure', high = '#2A9D8F',
    midpoint = 0,
    guide = guide_legend(
      title = element_blank(),
      label.position = 'top',
      keywidth = unit(1, 'cm'),
      keyheight = unit(.3, 'cm'),
      override.aes = list(color = NA))) +
  theme_map() +
  theme(
    text = element_text(color = 'red'),
    legend.direction = 'horizontal', 
    legend.background = element_blank(),
    legend.position = c(.2, .005),
    strip.background = element_blank(),
    strip.text = element_text(color = 'dodgerblue'),
    plot.background = element_rect(fill = 'black'),
    panel.background = element_rect(fill = 'black'),
    plot.margin = margin(t = 4, b = 0)) -> pw
pw2 <- pw + theme(legend.position = "none")
le1 <- cowplot::get_legend(pw)
cowplot::plot_grid(pw2, le1, nrow = 2, rel_heights = c(6, 1))
pw3 <- pw2 + plot_annotation(
  title = 'The Global Human Day: West Africa',
  subtitle = 'Hours per day combined for the country by  activity',
  caption = 'TidyTuesday | Week-37',
  theme = theme(plot.title = element_text(size = 18, hjust = .5),
                plot.subtitle = element_text(size = 14, hjust = .5))) +
  theme(text = element_text('roboto'))

ggdraw() +
  draw_plot(pw3) +
  draw_plot(le1, y = .1, x = .15)

ppp <- 
  (s_plot + r_plot) / (h_plot + m_plot) +
  plot_annotation(
    title = 'The Global Human Day: West Africa',
    subtitle = 'Hours per day combined for the country by  activity',
    caption = 'TidyTuesday | Week-37',
    theme = theme(plot.title = element_text(size = 18, hjust = .5),
                  plot.subtitle = element_text(size = 14, hjust = .5))) +
  theme(text = element_text('roboto'))

ppp

###
# social:
afr |> 
  filter(Subcategory == 'Social') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  theme_map() +
  labs(title = 'SOCIAL') -> s2

# Religious practice
afr |> 
  filter(Subcategory == 'Religious practice') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  theme_map() +
  labs(title = 'RELIGION') -> r2

# Hygiene & grooming  
afr |> 
  filter(Subcategory == 'Hygiene & grooming') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  theme_map() +
  labs(title = 'HYGIENE/GROOMING')  -> h2

# Meals 
#'#E76F51''#F4A261''#E9C46A''#2A9D8F''#264653''

afr |> 
  filter(Subcategory == 'Meals') |> 
  ggplot() +
  geom_sf(data = afr_no_data) +
  geom_sf(aes(fill = hoursPerDayCombined)) +
  theme_map() +
  labs(title = 'MEALS') -> m2



pp2 <- 
  (s2 + r2) / (h2 + m2) +
  plot_annotation(
    title = 'The Global Human Day: West Africa',
    subtitle = 'Hours per day combined for the country by  activity',
    caption = 'TidyTuesday | Week-37',
    theme = theme(plot.title = element_text(size = 18, hjust = .5),
                  plot.subtitle = element_text(size = 14, hjust = .5))) +
  theme(text = element_text('roboto'))
pp2
