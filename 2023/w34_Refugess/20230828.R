# Load packages -----------------------------------------------------------
pacman::p_load(
  tidyverse, janitor, scales, showtext, patchwork, ggtext, glue,
  ggstream, cowplot)


# Load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load("2023-08-22")
tuesdata

# Load fonts --------------------------------------------------------------
font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------
bg_col <- ""
text_col <- ""
highlight_col <- ""
cols_set1 <- c("#FFB400", "#FFC740", "#C20008", "#FF020D", "#13AFEF")
c("#27285C", "#0a7b8a", "#C20008", "#1035AC")
cols_set2 <- c('#18375f', '#00b398', '#ef4a60', '#0072bc',   '#8ebeff')
cols_set3 <- c('#FFC740', '#00b398', '#ef4a60', '#0072bc')

# Data wrangling ----------------------------------------------------------
df <- tuesdata$population

mali_df <-  df |> 
  filter(coo_name  == 'Mali') |> 
  select(year,
         country = coa_name,
         refugees,
         asylum_seekers,
         idps,
         stateless,
         ooc,
         oip
  )

mali_df |> glimpse()

plot_df <- mali_df |> 
  pivot_longer(-c(year, country), 
               names_to = 'type', 
               values_to = 'total',
               #values_drop_na = TRUE
  ) |> 
  mutate(
    type = case_when(
      type == 'asylum_seekers' ~ 'asylum seekers',
      type == 'idps' ~ 'internally displaced persons',
      type == 'ooc' ~ 'others of concern',
      type == 'oip' ~ 'other in need of interntl protection',
      TRUE ~ type
    )
  )

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

plot_df |> summarise_all(~ sum(is.na(.)))
plot_df |> filter(is.na(total)) |> nrow()

# by  year
by_year_df <- plot_df |> 
  filter(!is.na(total), total > 0) |> 
  group_by(year, type) |> 
  summarise(total = sum(total), .groups = 'drop') |> 
  mutate(type = str_to_title(type))

by_year_df |> 
  group_by(type) |> 
  summarise(total = sum(total, na.rm = TRUE))

by_year_df |>
  ggplot(aes(year, total, color = type)) +
  geom_line() +
  scale_x_continuous(
    breaks = c(2010, 2012, 2014, 2016, 2018, 2020, 2022),
    labels = c('2010', '2012', '2014', '2016', 
               '2018', '2020', '2022')) +
  scale_color_brewer(palette = 'Set2') +
  scale_y_continuous(labels = comma) +
  labs(x = '', y = 'Number od people',
       title = 'Forcibly displaced and stateless populations')  +
  theme_light()

by_year_df |> 
  ggplot(aes(year, total, fill = type)) +
  geom_stream(
    color = 1, 
    lwd = 0.25,
    n_grid = 1000
  ) +
  labs(fill  = NULL,
       x     = NULL,
       y     = NULL,
       title = 'Forcibly Displaced & Stateless Populations',
       subtitle = 'Mali Data from 2010 to 2022'
  ) +
  scale_fill_manual(values = cols_set3) +
  scale_x_continuous(
    breaks = c(2010, 2012, 2014, 2016, 2018, 2020, 2022),
    labels = c('2010', '2012', '2014', '2016', 
               '2018', '2020', '2022')) +
  #scale_y_continuous(labels = comma) +
  scale_y_continuous(
    breaks = seq(-4, 4, by = 2) * 1e5,
    labels = c('400K', '200K', '0', '200K','400K'))  +
  hrbrthemes::theme_ft_rc() +
  #theme_minimal() +
  theme(text = element_text(color = 'white'),
        axis.text = element_text(color = 'white'),
        plot.subtitle = element_text(color = '#ffb6c1', face = 'bold'),
        legend.position = c(1.3,.8),
        legend.key.height = unit(0.0001, 'line'),
        legend.background = element_rect(fill = '#353d4a'),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(1,10,1,1), "cm"))  -> main_plot


# by_type
by_type_df <- plot_df |> 
  filter(!is.na(total), total > 0) |> 
  group_by(type) |> 
  summarise(total = sum(total), .groups = 'drop') |> 
  mutate(
    type = str_to_title(type),
    fraction = total / sum(total),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    labelPosition = (ymax + ymin) / 2,
    label = paste0(round(fraction * 100, 2), '%'))

by_type_df |> 
  ggplot(aes(type, total)) +
  geom_col()

by_type_df |> 
  ggplot(aes(ymax = ymax, ymin = ymin, 
             xmax = 4, xmin = 3, 
             fill = type)) +
  geom_rect() +
  geom_label(
    x = 4.2,
    aes(y = labelPosition, label = label), size = 3) +
  #scale_fill_brewer(palette = 4) +
  scale_fill_manual(values = cols_set3) +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  labs(caption = 'Source: UNHCR Refugee Data Finder | r4ds-w34') +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,1,0), "cm"),
        plot.caption = element_text(
          color = 'white', 
          face = 'italic')) -> inset_plot


# Save plot ----------------------------------------------------------------
gg <- ggdraw() +
  draw_plot(main_plot) +
  draw_plot(inset_plot, x = 0.6, y = 0, width = .4, height = .4)
gg



ggsave(
  filename = file.path(new_folder, "refugees.png"),
  device = "png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)
getwd()
