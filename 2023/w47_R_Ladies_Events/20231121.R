
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, patchwork, plotly,
  showtext, ggtext, glue, shadowtext, ggalt)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 47)   #tidytuesdayR::tt_load('2023-11-21')
rlce <- tuesdata$rladies_chapters
rlce |> tail(10) |> view()

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Fira code", "firac")
font_add_google("Fira sans", "firas")
font_add_google("Fira mono", "firam")
font_add_google("Archivo Black", "archivo")
font_add_google("Eater", "eater")
font_add_google("Ubuntu", "ubuntu")
font_add_google("Lobster Two", "lobstertwo")
font_add_google("Caladea", "caladea")
font_add_google("Outfit", "outfit")
font_add_google(name = "IM Fell DW Pica", family = "IM Fell DW Pica")
pica <- "IM Fell DW Pica"
font_add_google(name = "Lato", family = "Lato")
lato <- "Lato"

showtext_auto()


# Define colors --------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""

tcols <- c("#476BA0", "#AA0000", "#B2ADA3",  "#68AA63",  
           "#A58C30", "#C9C977", "#DBD83D",  "#BAD8EA")
mcols <- c("#0073C2FF", "#EFC000FF", "#603C14", "#CD4F41", "#2D8077",
           "#BC8F8F", "#8FBC8F")
rcols <- c("#bb3e03","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#005f73")

pcols <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
           '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
# pcols: muted blue-safety orange-cooked asparagus green-brick red
#        muted purple-chestnut brown-raspberry yogurt pink
#        middle gray-curry yellow-green-blue-teal


#library(colorspace)
#cscols <- choose_palette()
#cscols

# Data wrangling ----------------------------------------------------------
df <- rlce |> 
  mutate(city = str_to_upper(str_remove_all(chapter, 'rladies-')),
         month = month(date, label = TRUE),
         day = wday(date, label = TRUE, abbr = FALSE),
         year = factor(year(date)),
         season = case_when(
           month %in% c('Dec', 'Jan', 'Feb') ~ 'Winter',
           month %in% c('Mar', 'Apr', 'May') ~ 'Summer',
           month %in% c('Jun', 'Jul', 'Aug') ~ 'Spring',
           TRUE ~ 'Autumn'
         )) |> 
  select(id, date, year, month, day, location, city, season)

df |> count(season)


location_month <- 
  df |> 
  count(month, location, name = 'total')
location_season <- 
  df |> 
  count(season, location, name = 'total')


### by City data -----
city_30 <- 
  df |> 
  filter(city != 'TUNIS') |> 
  count(city, location, sort = TRUE) |> 
  head(30) |> 
  pull(city) 

city_order <- rev(
  c("MELBOURNE", "DC", "TBILISI", "PHILLY", "NEWYORK","GUAYAQUIL",
    "COLUMBUS", "SEATTLE", "SYDNEY", "MONTREAL"))

df |> 
  count(city, sort = TRUE) |> 
  filter(city %in% city_order)


city_timing <- 
  df |> 
  filter(city %in% city_order) |> 
  mutate(timing = case_when(
    date < "2020-03-24" ~ 'Before',
    TRUE ~ 'After')) |>
  count(city, timing, name = 'total') |>
  group_by(city, timing) |> 
  summarise(total = sum(total), .groups = 'drop') |> 
  #ungroup() |> 
  pivot_wider(names_from = timing, values_from = total) |> 
  drop_na() |> 
  mutate(diffpct = (After - Before) / (After + Before)) |> 
  filter(diffpct >= -.4,
         diffpct <= .4) |>
  pivot_longer(-c(city,diffpct),
               names_to = 'timing', 
               values_to = 'total') |> 
  mutate(paired = rep(1:(n()/2),each = 2),
         city = factor(city, levels = city_order),
         timing = factor(timing, levels  = c('Before', 'After')),
         color = if_else(diffpct > 0, 'pov', 'neg'))


### lp_data: top_30_cities ------
top_30_cities <- df |> 
  count(city, sort = TRUE) |> 
  mutate(pct = n / sum(n)) |> 
  head(30)

llp_plot_data <- df |> 
  mutate(timing = case_when(
    date < "2020-03-24" ~ 'Before',
    TRUE ~ 'After')) |>
  count(city, timing, name = 'total') |>
  group_by(city, timing) |> 
  summarise(total = sum(total), .groups = 'drop') |> 
  arrange(desc(total)) |> 
  pivot_wider(names_from = timing, values_from = total) |> 
  drop_na() |> 
  mutate(check = if_else(After > Before, 'Y', 'N'))

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


# Plots ----------------------------------------------------------

## Bar plot ---------

### monthly  events  ----
monthly_events <- df |> 
  count(month)
pm <- monthly_events |> 
  ggplot(aes(month, n)) +
  geom_col(fill = pcols[1])

# basic bar plot
ggplotly(pm)
monthly_events |> plot_ly(x = ~month, y = ~n, type = "bar")


### yearly  events -----
yearly_events <- df |> 
  count(year)
py <- yearly_events |> 
  ggplot(aes(year, n)) +
  geom_col(fill = pcols[1])
py 
ggplotly(py)

yearly_events |> plot_ly(x = ~year, y = ~n, type = "bar")


### yearly events ggplot ------

title <- "<b style='color:#bb3e03; font-size:20pt'>R Ladies Events By Year</b>"
subt <- str_wrap(
  glue("The first",
       "<b style='color:#BC8F8F;'> INPERSON events </b>",
       "was held in <b>San-Francisco, CA</b> at the end of<b> 2012 </b>.",
       "<b style='color:#8FBC8F;'> ONLINE events </b>",
       "started in <b> 2020 </b>",
       "with the first event held on Mar 24th in <b>Gainesville, FA</b>."))
df |>
  count(year, location) |> 
  ggplot(aes(year, n, fill = location)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#BC8F8F", "#8FBC8F")) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = title,
       subtitle = subt,
       y = NULL, x = NULL) +
  theme_minimal() +
  theme(
    plot.title = element_textbox_simple(family = 'roboto'),
    plot.subtitle = element_textbox_simple(
      family = 'firam', 
      margin = margin(.3, 0, 0.5, 0, unit = 'cm')),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(family = 'firam'),
    
  )

## Diverging bar plot:  ggplotly --------
# Modified from this source: https://github.com/gkaramanis/tidytuesday/tree/master/2023/2023-week_46
###  location by month: count -----

by_month <- location_month |>
  mutate(total = if_else(location == "online", total, -total)) |>
  ggplot(aes(y = month, x = total, fill = location)) +
  geom_col(
    aes(color = after_scale(colorspace::darken(fill, 0.9))),
    linewidth = 0.4, width = .7) +
  geom_vline(xintercept = 0, color = 'grey90') +
  scale_fill_manual(values = c("#BC8F8F", "#8FBC8F")) +
  scale_x_continuous(
    labels = function(x) scales::number(abs(x)),
    limits = c(-270, 270)) +
  labs(
    title = "R LADIES EVENTS",
    subtitle = str_wrap("Most of the events were held in a inperson setting with the months of October, September and March being the months in which both inperson and online events are held.", 70),
    caption = "Source: Data from TidyTuesday - Week47 | Birusod") +
  theme_minimal(base_family = 'caladea') +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 20, 
                              hjust = .5, family = pica),
    plot.subtitle = element_text(size = 12, hjust = .5),
    plot.caption = element_text(margin = margin(15, 0, 0, 0)),
    plot.margin = margin(20, 20, 10, 20)
  )
by_month +
  geom_shadowtext(aes(x = 0, label = month), 
                  family = 'outfit', color = tcols[7], size = 5)
ggplotly(by_month) |>
  layout(
    title = list(
      text = paste0('R LADIES EVENTS',
         '<br>',
         '<sup>',
        'Most of the events were held in a inperson setting.',
        '</sup>', '<br>')))

###  location by season: percent -----

by_season <- location_season |>
  group_by(season) |> 
  mutate(pct = total / sum(total),
         pct = if_else(location == "online", pct, -pct)) |> 
  ggplot(aes(pct, season, fill = location)) +
  geom_col(
    aes(color = after_scale(colorspace::darken(fill, 0.9))),
    linewidth = 0, width = .5) +
  geom_vline(xintercept = 0, color = 'grey90') +
  scale_fill_manual(values = c(pcols[9], pcols[1])) +
  scale_x_continuous(
    labels = function(x) scales::percent(abs(x)),
    limits = c(-.7, .7)) +
  guides(color = 'none') +
  labs(
    title = "R LADIES EVENTS",
    subtitle = str_wrap("Most of the events were held in a inperson setting. Spring is the only season when the percentage for both inperson and online are balanced.", 90),
    caption = "Source: Data from TidyTuesday - Week47 | Birusod") +
  theme_minimal(base_family = 'caladea') +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    plot.background = element_rect(fill = "grey99", color = NA),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 16),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", size = 20, 
                              hjust = .5, family = pica),
    plot.subtitle = element_text(size = 12, hjust = .5),
    plot.caption = element_text(margin = margin(15, 0, 0, 0)),
    plot.margin = margin(20, 20, 10, 20)
  )

by_season + 
  geom_shadowtext(aes(x = 0, label = season, color = season), 
                  family = 'outfit', size = 10)

ggplotly(by_season) |>
  layout(
    title = list(
      text = paste0(
        'R LADIES EVENTS',   '<br>',
        '<sup>', 'Most of the events were held in a inperson setting.',
        '<br>', 'Spring is the only season where percentages are balanced.',
        '</sup>', '<br>', '<br>')))


## Funnel plot  --------
lbls = c(seq(-750, 0, 250), seq(250, 750, 250))
pct_lab <- c('60%', '40%', '20%', '0%', '20%', '40%', '60%')
cnt_lab <- c('750', '500', '250', '0', '250', '500', '750')
funnel_p <- 
  location_season |>
  mutate(total = if_else(location == "online", total, -total)) |> 
  ggplot(aes(x = reorder(season,abs(total)), 
             y = total, fill = location)) +
  geom_bar(stat = "identity", width = .4) +
  scale_fill_manual(values = c(pcols[9], pcols[1])) +
  scale_y_continuous(
    breaks = seq(-750, 750, by = 250), 
    limits = c(-750, 750),
    labels = cnt_lab) +
  coord_flip() +
  theme_minimal() +
  labs(x = '', y = '') +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank(),
        plot.margin = margin(2, 0, 1, 0, unit = 'cm'))

ggplotly(funnel_p) |> 
  layout(
    title = list(
      text = paste0(
        'TOTAL R LADIES EVENTS',   '<br>',
        '<sup>', 'Most of the events were held in a inperson setting.',
        '<br>', 'Spring is the only season where percentages are balanced.',
        '</sup>', '<br>', '<br>')))

## Dumbbell Plot  -----
# not an insightful plot but it was nice learn to compare 2 time-events
# before and after Covid-19

# ggplot2:
city_title <- glue(
  "<b>EXPLORING  TOP 10 CITIES BY FREQUENCY</b>")
city_subt <- str_wrap(
  glue(
    "Comparing number of events",
    "<b style='color:#9467bd;'> before </b>", "and",
    "<b style='color:#603C14;'> after </b>",
    "Covid-19 start ",
    "(<b>March  2020</b>)"))

dumbbell_plot <- city_timing |>
  ggplot(aes(total, city)) +
  geom_segment(aes(
    x = 0, xend = total,
    y = city, yend = city), linewidth = .05, linetype = 'dotted') +
  geom_line(aes(group = paired, color = color), linewidth = 2) +
  geom_point(aes(fill = timing), size = 6, shape =  21, stroke = 0) +
  scale_color_manual(
    values = c('#CD4F41', '#94d2bd'),
    guide = 'none') +
  scale_fill_manual(values = c('#9467bd', '#603C14')) + 
  labs(fill = '', x = '', y = '',
       title = city_title,
       subtitle = city_subt) +
  theme_minimal(base_size = 12,
                base_family = 'roboto') +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(color = 'navy'),
    plot.subtitle = element_textbox_simple(
      margin = margin(.3, 0, 0.5, 0, unit = 'cm')),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(linetype = 'dashed'),
    axis.text = element_text(color = 'grey50', face = 'bold')
    )
dumbbell_plot

dumbbell_plot_2 <- city_timing |>
  ggplot(aes(total, city)) +
  geom_line(aes(group = paired), color = 'grey70', linewidth = 2) +
  geom_point(aes(fill = timing), size = 6, shape =  21, stroke = 0) +
  scale_fill_manual(values = c('#9467bd', '#603C14')) +
  theme_classic() +
  theme(plot.margin = margin(2, 2, 1, 2, unit = 'cm'),
        axis.ticks = element_blank(),
        axis.line = element_line(color = 'grey80'),
        panel.grid.major.x = element_line(linetype = "dotted", color = 'grey80'))

ggplotly(dumbbell_plot_2)  |> 
  layout(
    title = list(
      text = paste0(
        "<b style='color:#CD4F41;'>EXPLORING  TOP 10 CITIES BY FREQUENCY</b>",
        "<br>", "<sup>",
        "Comparing number of events<b style='color:#9467bd;'> before </b>", 
        "and", "<b style='color:#603C14;'> after </b>",
        "Covid-19 start ", "(<b>March  2020</b>)", "</sup>", "<br>", "<br>"),
      font = list(size = 25, family = 'roboto')),
    legend = list(
      title = list(
        text = 'Event timing',
        font = list(size = 20, family = 'roboto', color = 'crimson'))),
    xaxis = list(title = 'Number of events',
                 ticks = "inside",
                 zerolinecolor = '#ffff',
                 zerolinewidth = 1,
                 gridcolor = 'grey80'),
    yaxis = list(title = ''),
    plot_bgcolor = '#ede5ce')
  

# ggalt
llp_plot_data |> 
  filter(check  == 'Y') |> 
  ggplot(aes(
    x = Before, xend = After, 
    y = fct_reorder(city, Before), group = city)) +
  geom_dumbbell(colour = "#a3c4dc", 
                size = 1.5, 
                colour_xend = "#0e668b", 
                dot_guide = TRUE, 
                dot_guide_size = 0.15)  +
  labs(
    x = NULL, y = NULL, 
    title = '50 Cities That Had More Events After Covid Than Before') +
  theme_bw() +
  theme(plot.background = element_rect(fill = "#f7f7f7"),
        panel.background = element_rect(fill = "#f7f7f7"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(),
        axis.ticks = element_blank(),
        legend.position = "top",
        panel.border = element_blank())


## Lolliplot --------


top_30_cities |> 
  ggplot(aes(n, fct_reorder(city, n))) +
  geom_lollipop(
    point.colour = "steelblue", 
    point.size = 3, horizontal = TRUE) +
  scale_x_continuous(
    expand = c(0,0), 
    breaks = seq(0, 150, by = 30), 
    limits = c(0, 148)) +
  labs(x = NULL, y = NULL, 
       title = "R LADIES EVENTS LOCATION",
       subtitle = "Top 30 cities (Online & Inperson combined)",
       caption = "Source: Data from TidyTuesday - Week47 | Birusod") +
  theme_minimal(base_family = "roboto") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(color = "#2b2b2b", size = 0.15),
        axis.text.y = element_text(margin = margin(r = 0, l = 0)),
        plot.margin = unit(rep(30, 4), "pt"),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10)))
  
  
# ggplotly(llp_plot) => error


## Pie and Donut chart  -------------

df |> 
  count(season, name = 'total') |> 
  plot_ly(
    type = 'pie',
    labels = ~season, 
    values = ~total,
    textinfo = 'label+value+percent'
    #hole = .4
    #insidetextorientation='radial'
  ) |> layout(title =  "Pie chart using Plotly", showlegend = F)


df |> 
  count(day, name = 'total') |> 
  plot_ly(labels = ~day, 
          values = ~total,
          textinfo = 'value+percent') |> 
  add_pie(hole = 0.6) |> 
  layout(
    title =  "Donut charts using Plotly (N, %)",
    xaxis = list(showgrid = FALSE, 
                 zeroline = FALSE, 
                 showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, 
                 zeroline = FALSE, 
                 showticklabels = FALSE))


# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-11-21", paste0("20231121", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )