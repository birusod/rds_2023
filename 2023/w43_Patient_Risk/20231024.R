
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, 
  showtext, patchwork, ggtext, glue, patchwork,
  ggpubr, # ggdonutchart
  webr # PieDonut
  )



# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-10-24")
prp <- tuesdata$patient_risk_profiles

# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col1 <- "wheat"
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------

by_gender <- prp |> 
  select(personId, contains('MALE')) |> 
  pivot_longer(-personId, names_to = 'gender', values_to = 'count') |> 
  mutate(gender = str_remove_all(gender, 'Sex = ')) |> 
  group_by(gender) |> 
  summarise(total = sum(count))

by_age <- prp |> 
  select(personId, contains('age group')) |> 
  pivot_longer(-personId, names_to = 'group', values_to = 'count') |> 
  mutate(group = str_remove_all(group, 'age group:  ')) |> 
  mutate(group = parse_number(group)) |> 
  mutate(age_group = case_when(
    group <= 15 ~ 'TEEN',
    group <= 60 ~ 'ADULT',
    TRUE ~ 'SENIOR')) |> 
  mutate(age_group = fct_relevel(age_group, 'TEEN', 'ADULT', 'SENIOR')) |>
  group_by(age_group) |> 
  summarise(total = sum(count)) 
  

gender_age_df <- prp |> 
  select(personId, contains('MALE')) |> 
  pivot_longer(-personId, names_to = 'gender', values_to = 'count') |> 
  mutate(gender = str_remove_all(gender, 'Sex = ')) |> 
  filter(count == 1) |> 
  inner_join(
    prp |> 
      select(personId, contains('age group')) |> 
      pivot_longer(-personId, names_to = 'group', values_to = 'count') |> 
      mutate(group = str_remove_all(group, 'age group:  ')) |> 
      mutate(group = parse_number(group)) |> 
      mutate(
        age_group = case_when(
          group <= 15 ~ 'TEEN',
          group <= 60 ~ 'ADULT',
          TRUE ~ 'SENIOR')) |> 
      mutate(age_group = fct_relevel(age_group, 'TEEN', 'ADULT', 'SENIOR')) |> 
      select(personId, age_group, count) |> 
      filter(count == 1) ,
    by = join_by(personId),
    relationship = "many-to-many"
    ) |> 
  group_by(gender,  age_group) |> 
  summarise(total = sum(count.x))

gender_age_df

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

# pie chart
by_gender |> 
  ggplot(aes(x = "", y = total, fill = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(gender, "\n", round(total/sum(total) * 100), "%")),
    position = position_stack(vjust = 0.5),
    size = 5, fontface = 'bold', family = 'roboto') +
  scale_fill_brewer(
    palette = "Dark2",
    guide = guide_legend(
      direction = "horizontal",
      title.position = "top",
      label.position = "bottom",
      title.hjust = .5,
      label.hjust = .5,
      label.vjust = 1,
      keywidth = 4
    )) +
  labs(title = 'GENDER DISTRIBUTION', fill  = 'GENDER') +
  theme_void() +
  theme(
    plot.title = element_text(
      family = 'roboto', 
      face = 'bold', size = 20,
      color = 'firebrick', hjust = .5),
    legend.position = 'none'
  )  -> p1

# donut

by_gender |> 
  ggplot(aes(x = "", y = total, fill = gender)) +
  geom_bar(stat = "identity", width = 1, fill = NA)  +
  geom_bar(aes(x = "", y = total, fill = gender), 
           stat = "identity", 
           width = .3) +
  coord_polar(theta = "y")  +
  geom_text(
    aes(label = paste0(gender, "\n", round(total/sum(total) * 100), "%")),
    position = position_stack(vjust = 0.5),
    size = 5, fontface = 'bold') +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = 'GENDER DISTRIBUTION', fill  = 'GENDER') +
  theme_void() +
  theme(
    plot.title = element_text(
      family = 'roboto', 
      face = 'bold', size = 20,
      color = 'firebrick', hjust = .5,
      margin =  margin(b = -3, t = .5, unit = 'cm')),
    legend.position = 'none',
    plot.background = element_rect(fill = bg_col1)
  )  -> p2


# by age
by_age |> 
  ggplot(aes(age_group, total)) +
  geom_col(show.legend = FALSE, fill = 'firebrick')  +
  labs(title = 'AGE DISTRIBUTION', x  = '', y  = 'Frequency') +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 57)) +
  theme_light() +
  theme(
    plot.title = element_text(
      family = 'roboto', 
      face = 'bold', size = 20,
      color = 'firebrick', hjust = .5),
    axis.text = element_text(size = 12, face = 'bold', family = 'roboto'),
    axis.title = element_text(size = 18, face = 'bold', family = 'roboto'),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )  -> p3

by_age |> 
  mutate(hole = 2) |> 
  mutate(pct = round(total / sum(total) * 100, 1)) |> 
  ggplot(aes(x = hole, y = pct, fill = age_group)) +
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = paste0(age_group, '\n', pct, '%')),
             position = position_stack(vjust = 0.7),
             show.legend = FALSE,
             fill = NA) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Dark2") +
  xlim(c(0.1, 2 + 0.5)) +
  labs(title = 'AGE DISTRIBUTION', x  = '', y  = 'Frequency') +
  theme_void() +
  theme(
    plot.title = element_text(
      family = 'roboto', 
      face = 'bold', size = 20,
      color = 'firebrick', hjust = .5))   -> p4



library(patchwork)
(p1 + p2)
(p3 + p4)


# https://rpkgs.datanovia.com/ggpubr/index.html
by_age |> 
  mutate(lab = paste0(age_group, '\n', total, '%')) |> 
  ggdonutchart(
    "total",
    fill = "age_group", 
    label = 'lab',
    ) |> 
  ggpar(legend.title = "AGE GROUPS")

by_age |> 
  mutate(lab = paste0(age_group, '\n', total, '%')) |> 
  ggdonutchart(
    "total", 
    label = 'lab',
    lab.pos = "in", 
    lab.font = c(5, "white", 'bold'),
    fill = "age_group", 
    color = "white",
    palette = c("#00AFBB", "#E7B800", "#FC4E07")) |> 
  ggpar(title = "AGE DISTRIBUTION",
        legend = 'none',
        tickslab  = FALSE
        ) +
  theme(plot.title = element_text(
    family = 'roboto', 
    face = 'bold', size = 20,
    color = 'firebrick', hjust = .5))




# Webr : 
PieDonut(
  gender_age_df, 
  aes(gender, age_group, count = total))

gender_age_df |> 
  PieDonut(
    aes(gender, age_group, count = total), 
    title = "AGE DISTRIBUTION",
    ratioByGroup = FALSE)

gender_age_df |> 
  PieDonut(
    aes(gender, age_group, count = total), 
    title = "GENDER DISTRIBUTION\nBY AGE GROUP",
    family = 'roboto',
    r0 = 0.5, r1 = 0.9)

# Save gif ----------------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-10-24", paste0("20231024", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
