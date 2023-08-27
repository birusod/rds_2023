
# Load packages -----------------------------------------------------------

pacman::p_load(tidyverse, janitor, scales, showtext, patchwork, ggtext, glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-08-29")
tuesdata

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------

tuesdata$fair_use_cases |> glimpse()
tuesdata$fair_use_findings |> glimpse()

cases <- tuesdata$fair_use_cases 
findings <- tuesdata$fair_use_findings |> mutate(year = parse_double(year))
cases |> 
  filter(
    case == 'Matthew Lombardo and Who’s Holiday LLC v. Dr. Seuss Enterprises, L.P.') |> 
  select(3)
findings[60, 3] <- 2018   # 2017, affirmed 2018
findings |> filter(is.na(year)) 


cases |> count(court, sort = TRUE)
cases |> count(jurisdiction, sort = TRUE)
cases |> count(fair_use_found, sort = TRUE)
cases |> count(outcome, sort = TRUE)
cases |> colnames()


findings |> count(court, sort = TRUE)
findings |> count(outcome, sort = TRUE)
findings |> colnames()

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

cases |> 
  mutate(court = fct_lump(court, 10)) |> 
  filter(fair_use_found) |> 
  count(court) |> 
  mutate(court  = fct_reorder(court, n)) |> 
  ggplot(aes(n, court, fill  =  if_else(court == 'Other', 'yes', 'no'))) +
  geom_col(show.legend = FALSE) +
  labs(y = '', x = '', 
       title = 'By Court',
       subtitle = 'Fair Use Found') +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c('dodgerblue', 'grey')) +
  theme_light() +
  theme(
    plot.title = element_text(face = 'bold', size = 20, color = '#DC143C'),
    plot.subtitle = element_text(face = 'bold', size = 15, color = 'dodgerblue')
    ) -> p1

cases |> 
  mutate(court = fct_lump(court, 10)) |> 
  filter(!fair_use_found) |> 
  count(court) |> 
  mutate(court  = fct_reorder(court, n)) |> 
  ggplot(aes(n, court, fill  =  if_else(court == 'Other', 'yes', 'no'))) +
  geom_col(show.legend = FALSE) +
  labs(y = '', x = '', subtitle = 'Fair Use Not Found') +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c('firebrick', 'grey')) +
  theme_light() +
  theme(
    plot.subtitle = element_text(
      face = 'bold', size = 15, color = 'firebrick')) -> p2

cases |> 
  mutate(jurisdiction = fct_lump(jurisdiction, 10)) |> 
  filter(fair_use_found) |> 
  count(jurisdiction) |> 
  mutate(jurisdiction  = fct_reorder(jurisdiction, n)) |> 
  ggplot(aes(n, jurisdiction, fill  =  if_else(jurisdiction == 'Other', 'yes', 'no'))) +
  geom_col(show.legend = FALSE) +
  labs(y = '', x = '', 
       title = 'By Jurisdiction',
       subtitle = 'Fair Use Found') +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c('skyblue', 'grey')) +
  theme_light() +
  theme(
    plot.title = element_text(face = 'bold', size = 20, color = 'navy'),
    plot.subtitle = element_text(face = 'bold', size = 15, color = 'skyblue')
  ) -> p3

cases |> 
  mutate(jurisdiction = fct_lump(jurisdiction, 10)) |> 
  filter(!fair_use_found) |> 
  count(jurisdiction) |> 
  mutate(jurisdiction  = fct_reorder(jurisdiction, n)) |> 
  ggplot(aes(n, jurisdiction, fill  =  if_else(jurisdiction == 'Other', 'yes', 'no'))) +
  geom_col(show.legend = FALSE) +
  labs(y = '', x = '', subtitle = 'Fair Use Not Found') +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c('#c4626e', 'grey')) +
  theme_light() +
  theme(
    plot.subtitle = element_text(
      face = 'bold', size = 15, color = '#c4626e')) -> p4


# Save gif ----------------------------------------------------------------


p <- (p1 + p3) / (p2 + p4)
p

ggsave(
  filename = file.path(new_folder, "fair_use.png"),
  device = "png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)
getwd()