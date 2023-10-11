
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, 
  showtext, patchwork, ggtext, glue,
  circlize)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-03")
tuesdata

grants <- tuesdata$grants
opp_details <- tuesdata$grant_opportunity_details

#opp_details |> head(30) |> view()

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------

posted_by_date <- 
  grants |> 
  filter(opportunity_status  == 'Posted') |> 
  select(posted_date, agency_code, agency_name, estimated_funding)


by_category <- opp_details |> 
  select(posted_date, agency_name, 
         starts_with('category'), 
         -category_explanation) |> 
  mutate(across(-c(1:2), \(x) as.integer(x))) |> 
  pivot_longer(
    starts_with('category'), 
    names_to = 'category', 
    values_to = 'count')


agency_other <- by_category |> 
  group_by(agency_name) |> 
  summarise(total = sum(count)) |> 
  filter(total < 20) |> 
  pull(agency_name)

category_other <- by_category |> 
  group_by(category) |> 
  summarise(total = sum(count)) |> 
  arrange(desc(total)) |> 
  filter(total < 20) |> 
  pull(category)


by_category_2 <- 
  by_category |> 
  mutate(
    agency_name = case_when(
      agency_name %in% agency_other ~ 'Other agencies',
      TRUE ~ agency_name),
    category = case_when(
      category %in% agency_other ~ 'Other agencies',
      TRUE ~ category)) |> 
  group_by(agency_name, category) |> 
  summarise(total = sum(count), .groups = 'drop') |> 
  arrange(desc(total))

by_category_3 <- 
  by_category |> 
  mutate(agency_name = str_remove_all(agency_name, ' -'),
         category = str_remove_all(category, 'category_'),
         category = str_replace_all(category, '_', ' '))


cat10 <- by_category_3 |> 
  group_by(category) |> 
  summarise(total = sum(count)) |> 
  arrange(desc(total)) |> 
  slice_head(n =  10) |> 
  pull(category)
agc3 <- by_category_3 |> 
  group_by(agency_name) |> 
  summarise(total = sum(count)) |> 
  arrange(desc(total)) |> 
  slice_head(n =  3) |> 
  pull(agency_name)



# keeping only a small number of categories/agencies and lumping the rest as other

agency_top <- by_category |> 
  group_by(agency_name) |> 
  summarise(total = sum(count)) |> 
  filter(total < 40) |> 
  pull(agency_name)

category_top <- by_category |> 
  group_by(category) |> 
  summarise(total = sum(count)) |> 
  arrange(desc(total)) |> 
  filter(total < 100) |> 
  pull(category)


by_category_4 <- 
  by_category_3 |> 
  filter(agency_name  %in% agc3, category %in% cat10) |> 
  #mutate(
    # agency_name = case_when(
    #   agency_name %in% agency_top ~ 'Other agencies',
    #   TRUE ~ agency_name),
    # category = case_when(
    #   category %in% agency_top ~ 'Other agencies',
    #   TRUE ~ category)) |>
  group_by(agency_name, category) |> 
  summarise(total = sum(count), .groups = 'drop') |> 
  filter(total > 0)

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


# posted by  date
posted_by_date |> 
  ggplot(aes(posted_date)) +
  geom_histogram() +
  labs(y = 'Frequency', x = '', 
       title = 'Agencies Distribution By Grants  Posted')


# by agency
posted_by_date |> 
  mutate(agency_code = fct_lump_n(agency_code, 20)) |> 
  count(agency_code) |> 
  ggplot(aes(n, fct_reorder(agency_code, n))) +
  geom_col() +
  labs(y = '', x = '', title = 'Agencies Distribution By Grants Posted') +
  theme(
    plot.title = element_text(
      family = 'roboto', size = 30, 
      face = 'bold',  color = 'brown')
  ) -> p1



# by category
by_category_2 |> 
  group_by(agency_name) |> 
  summarise(total = sum(total)) |> 
  mutate(agency_name = fct_reorder(agency_name, total)) |> 
  ggplot(aes(total, agency_name)) +
  geom_col()

by_category_2 |> 
  group_by(category) |> 
  summarise(total = sum(total)) |> 
  mutate(category = fct_reorder(category, total)) |> 
  ggplot(aes(total, category)) +
  geom_col()


# circulize
pacman::p_load(circulize)

by_category_4 |> chordDiagram()  

# webr
pacman::p_load(webr)

by_category_4 |> PieDonut(aes(x = agency_name, count = total))
by_category_4 |> PieDonut(
  aes(x = category, count = total),
  title = 'Categories Distribution By Grants Posted')

# plotly
pacman::p_load(plotly)
colors <- c("red", "orange", "yellow", "green", "blue")
by_category_4 |> 
  plot_ly(labels = ~category, 
          values = ~total, 
          type = "pie",
          hole = 0.6) |> 
  layout(title = "Grants By Category",
         legend = list(
           x = 0, y = 1,
           font = list(size = 10, color = "black"),
           orientation = "v",
           xanchor = 'center'
         )) -> p3

cols <- c("red", "orange", "cyan")
mkr <- list(colors = cols)
by_category_4 |> 
  plot_ly(labels = ~agency_name, 
          values = ~total, 
          type = "pie",
          hole = 0.5) |> 
  layout(
    title = "Top 3 Agencies with most grants",
    legend = list(
      x = 0.5, y = 0,
      font = list(size = 10, color = "black"),
      orientation = "h",
      xanchor = 'center'
    )) -> p4
        





# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-10-03", paste0("20231003", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )

dir = paste0(here::here(), '/2023/w40_USGovGrants')

ggsave(
  paste0(dir, '/grants_by_agency.png'), 
  p1,
  units = "in", 
  dpi = 150)
