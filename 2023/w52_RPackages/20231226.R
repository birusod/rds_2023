
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue)


# Loading data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-12-26")
rpkg <- tuesdata$cran_20221122
rpkg |> head(10) |> view()
# Loading fonts ------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Fira code", "firac")
font_add_google("Fira sans", "firas")
showtext_auto()


# Defining colors ---------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""

pcols <- c('#1f77b4', '#AA0000', '#9467bd', '#8c564b')


# Wrangling Data ----------------------------------------------------------

imported <- rpkg |> 
  select(package, imports) |> 
  separate_rows(imports) |> 
  filter(!is.na(imports)) 

suggested <- rpkg |> 
  select(package, suggests) |> 
  separate_rows(suggests) |> 
  filter(!is.na(suggests))

depends <- rpkg |> 
  select(package, depends) |> 
  separate_rows(depends) |> 
  filter(!is.na(depends))

enhances <- rpkg |> 
  select(package, enhances) |> 
  separate_rows(enhances) |> 
  filter(!is.na(enhances))

### test ------
imported |> 
  full_join(suggested, 
            by = join_by(package),
            relationship = "many-to-many") |> 
  drop_na()

imported |> 
  count(package) |> 
  full_join(suggested |> count(package), 
            by = join_by(package),
            suffix = c("imports", "suggests")) |> 
  rename(imports  = nimports, suggests = nsuggests) |> 
  group_by(package) |> 
  mutate(total = sum(imports, suggests, na.rm = TRUE)) |> 
  arrange(desc(total)) |> head(20) |> view()


### combined data -----

df_merged <- rpkg |> 
  count(package) |> 
  left_join(imported  |> count(package, name  = 'imports'))  |> 
  left_join(suggested |> count(package, name  = 'suggests')) |> 
  left_join(depends   |> count(package, name  = 'depends'))  |> 
  left_join(enhances  |> count(package, name  = 'enhances')) |> 
  select(-n) |> 
  pivot_longer(-package, names_to = 'cat', values_to = 'count') |> 
  group_by(package) |> 
  mutate(total = sum(count))

df_r1 <- df_merged |> 
  arrange(desc(total)) |> 
  top_n(n = 1, wt = total) |>
  ungroup()
  

# Define texts and annotations --------------------------------------------

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

plot_top20 <- function(data, grpvar, title = 'title',
                       subtitle = 'subtitle', caption = 'cap', 
                       pcolor, ...){
    data |> 
      count({{ grpvar }}, sort = TRUE) |> 
      mutate(gvar := fct_reorder(str_to_upper({{ grpvar }}), n)) |> 
      head(20) |> 
      ggplot(aes(n, gvar)) +
      geom_col(fill = pcolor)  +
      scale_x_continuous(expand = c(0, 0), labels = comma) +
      labs(title = title, caption = caption, subtitle =  subtitle,
           x = NULL, y = NULL) +
      theme_base(base_size = 12) +
      theme(plot.subtitle = element_text(color = pcolor))
}

mytheme <- theme_gppr <- function(pcolor){ 
  font <- "roboto"   
  theme_minimal() %+replace%    #replace elements to change
    theme(
      panel.grid.major.y = element_blank(),   
      panel.grid.minor = element_blank(),    
      axis.ticks = element_blank(),          
      plot.title = element_text(family = font, size = 20,
                                face = 'bold',  hjust = 0, vjust = 2),
      plot.subtitle = element_text(family = font, size = 16, color = pcolor,
                                face = 'bold.italic',  hjust = 0, vjust = 2),
      plot.caption = element_text(family = font, size = 9, 
                                  hjust = 1, color = 'grey50'), 
      axis.title = element_text(family = font, size = 10), 
      axis.text = element_text(family = font, size = 9, face = 'bold'),   
      plot.margin = (margin = margin(1, 1, 1, 1, unit = 'cm'))
    )
}
pcols_ordered <- c('#8c564b', '#9467bd', '#AA0000', '#1f77b4' )
suggested |> plot_top20(
  suggests, 
  title = "Top 20 Packages",
  subtitle = "Count Of Packages Susgested",
  caption = 'TidyTuesday Data | Week-52',
  pcolor = pcols[1]) +
  mytheme(pcolor = pcols[1])

imported |> plot_top20(
  imports, 
  title = "Top 20 Packages",
  subtitle = "Count Of Packages Imported",
  caption = 'TidyTuesday Data | Week-52',
  pcolor = pcols[2]) +
  mytheme(pcolor = pcols[2])

depends |> plot_top20(
  depends, 
  title = "Top 20 Packages",
  subtitle = "Count Of Packages Classified as Depends",
  caption = 'TidyTuesday Data | Week-52',
  pcolor = pcols[3]) +
  mytheme(pcolor = pcols[3])

enhances |> plot_top20(
  enhances, 
  title = "Top 20 Packages",
  subtitle = "Count Of Packages Classified as Enhances",
  caption = 'TidyTuesday Data | Week-52',
  pcolor = pcols[4]) +
  mytheme(pcolor = pcols[4])
  
  
df_r1 |> 
  mutate(package =  fct_reorder(str_to_upper(package), total),
         cat = str_to_upper(cat)) |> 
  ggplot(aes(count, package, fill = cat))  +
  scale_fill_manual(values = pcols_ordered)  +
  geom_col() + # position = 'fill'
  labs(fill = NULL, y = NULL, x = NULL, 
       title = 'TOP R PACKAGES',
       subtitle = 'By total number of packages classified as imports,\nsuggests, enhances and depends') +
  #scale_x_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 41)) +
  guides(fill = guide_legend(
    label.position = 'top',
    direction  = 'horizontal',
    reverse = TRUE
    
  )) +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = 'grey80', linetype = 'dashed'),
        legend.position = c(.6, .1), #'top',
        legend.background = element_blank(),
        plot.title = element_text(face = 'bold', color =  'black', size = 20),
        plot.subtitle = element_text(face = 'bold.italic', color =  'grey40', size = 16),
        plot.margin = (margin = margin(0.5, 1, 0.5, 1, unit = 'cm')),
        axis.text = element_text(face = 'bold')
        )
  

# Saving Plots and Gifs ------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-12-26", paste0("20231226", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
