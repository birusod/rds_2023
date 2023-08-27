
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
  mutate(court = fct_lump_n(court, n = 10)) |> 
  count(court, sort = TRUE) |> 
  ggplot(aes(n, fct_reorder(court, n))) +
  geom_col()

 cases |> 
   count(jurisdiction, sort = TRUE) |> 
   ggplot(aes(n, fct_reorder(jurisdiction, n))) +
   geom_col()

cases |> 
  count(fair_use_found, sort = TRUE) |> 
  ggplot(aes(fair_use_found, n)) +
  geom_col()

cases |> 
  mutate(court = fct_lump_n(court, n = 10)) |> 
  count(court, fair_use_found, sort = TRUE) |> 
  ggplot(aes(n, fct_reorder(court, n), fill = fair_use_found)) +
  geom_col(position = 'dodge') +
  labs(fill = 'Fair Use Found', y = '', x = '',
       title  = 'Cases Distribution by  Court') +
  scale_x_continuous(expand = c(0,0)) +
  theme_light() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

cases |> 
  count(jurisdiction, fair_use_found, sort = TRUE) |> 
  ggplot(aes(n, fct_reorder(jurisdiction, n), fill = fair_use_found)) +
  geom_col(position = 'dodge') +
  labs(fill = 'Fair Use Found', y = '', x = '',
       title  = 'Cases Distribution by  Jurisdiction') +
  scale_x_continuous(expand = c(0,0)) +
  theme_light() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))


# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-08-29", paste0("20230829", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
