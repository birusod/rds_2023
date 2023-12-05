
# Load packages -----------------------------------------------------------
pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, 
  showtext, patchwork, ggtext, glue,
  gt, gtsummary, gtExtras, DT, reactable, reactablefmtr,
  huxtable, kableExtra, flextable)


# Loading data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-28")
episodes <- tuesdata$drwho_episodes 
directors <- tuesdata$drwho_directors
writers <- tuesdata$drwho_writers
# Loading fonts ------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Eater", "eater")
font_add_google("Fira code", "firac")
showtext_auto()



# Defining colors ---------------------------------------------------------
bg_col <- ""
text_col <- ""
highlight_col <- ""

# Wrangling Data ----------------------------------------------------------

## df final ----
df <- episodes |> 
  filter(type == 'episode') |>  
  select(-c(era, serial_title, type),
         date = first_aired,
         season = season_number, 
         episode = episode_number,
         title = episode_title,
         code = production_code,
         viewers =  uk_viewers) |> 
  left_join(directors, by = join_by(story_number)) |> 
  left_join(writers, by = join_by(story_number)) |> 
  mutate(
    year = year(date),
    month  = month(date, label = TRUE, abbr = TRUE),
    day  = wday(date, label = TRUE, abbr = TRUE),
    season = case_when(
      month %in% c('Dec', 'Jan', 'Feb') ~ 'Winter',
      month %in% c('Mar', 'Apr', 'May') ~ 'Summer',
      month %in% c('Jun', 'Jul', 'Aug') ~ 'Spring',
      TRUE ~ 'Autumn'),
    pos = case_when(
      rating <= 80 ~ 'Low',
      TRUE ~ 'High'
    ))

# tables data
by_season <- df |> 
  group_by(season, pos) |> 
  summarise(avg = mean(rating), .groups = 'drop')

directors_tab <- df |> 
  mutate(viewers = round(viewers * 1e6), 0) |> 
  unite(se, c(season, episode), sep = '.') |> 
  select(director, date, title, viewers, rating, duration) |> 
  rename_with(str_to_upper)
#column_to_rownames(var = 'date') not unique


writers_tab <- df |> 
  mutate(viewers = round(viewers * 1e6), 0) |> 
  unite(se, c(season, episode), sep = '.') |> 
  select(writer , date, title, viewers, rating, duration) |>
  rename_with(str_to_upper)


## fake data ----
seq = sample.int(100, 100)
fake_data <- 
  tibble(
    director = rep(unique(df$director)[1:10], each = 3),
    rating  = sample(seq[seq > 50], 30, replace = FALSE)) |>
  group_by(director) |> 
  mutate(position = case_when(
    rating == min(rating) ~ 'Low',
    rating == max(rating) ~ 'High',
    TRUE ~ 'Medium')) |> 
  mutate(
    position = fct_relevel(
      position, 'Low', 'Medium', 'High'))
 
fake_data


## overall success -----
d10 <- df |> 
  arrange(desc(rating)) |> 
  select(director, rating) |> 
  filter(!duplicated(director)) |> 
  head(10) |> 
  pull(director)

success_df <- df |> 
  filter(!duplicated(director)) |>  
  filter(director %in% d10) |> 
  arrange(desc(rating)) |> 
  select(director, season, viewers, rating) |>
  mutate(viewers = viewers * 1e6,
         success = rating) |> 
  group_by(season) |> 
  mutate(avg = mean(rating)) |>
  rename_with(str_to_title)

## Flex timing -----
ftm <- df |> 
  select(director, date, season, viewers:duration, pos) |> 
  mutate(pos = fct_relevel(pos, 'Low', 'High')) |> 
  group_by(pos) |> 
  arrange(desc(viewers)) |> 
  filter(!duplicated(director)) |> 
  slice_head(n = 6) |> 
  ungroup()


## React data -----

rtb <- df |> 
  select(director, date, season, viewers:duration, pos) |> 
  mutate(pos = fct_relevel(pos, 'Low', 'High')) |> 
  group_by(pos) |> 
  arrange(desc(viewers)) |> 
  filter(!duplicated(director)) |> 
  head(n = 50) |> 
  ungroup() |> 
  rename_with(str_to_title)

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

## Yearly  Rating Average ----
df |> 
  group_by(year) |> 
  summarise(avg = mean(rating))

df |> 
  group_by(year) |> 
  summarise(avg = mean(rating)) |> 
  ggplot(aes(year, avg)) +
  geom_col()

## rating by year ------

df |> 
  ggplot(aes(factor(year), rating)) +
  geom_boxplot()

## rating by viewers (millions)

df |> 
  ggplot(aes(viewers, rating)) +
  geom_point()


## Tables -------

df |> colnames()


directors_tab |> 
  filter(DIRECTOR == 'Alice Troughton')



### Datatable -----

directors_tab |> 
  datatable(
    rownames = FALSE,
    caption = 'Listing Of Doctor Who Episodes By Directors',
    class = 'cell-border stripe hover',
    options = list(
      columnDefs = list(
        list(width = '130px', targets = c(1)),
        list(width = '400px', targets = c(2)),
        list(width = '100px', targets = c(4,5))))) |> 
  formatStyle("DIRECTOR", 
              target = 'row',
              backgroundColor = "coral",
              color = "darkblue") |> 
  formatStyle(
    names(directors_tab),
    lineHeight = '80%')

writers_tab |> 
  datatable(
    class = "compact",
    filter = 'top', 
    options = list(
      pageLength = 10, 
      autoWidth = TRUE),
    caption = htmltools::tags$caption(
      style = 'caption-side:top; text-align:center; color:crimson; font-size:20pt', 
      htmltools::strong(
        'Listing Of Doctor Who Episodes By Writors'
        )
      )
    )


### gt &  gtExtras -------

#### advanced gt table ----
directors_tab |> 
  arrange(desc(VIEWERS)) |> head(10) |> 
  gt() |> 
  tab_header(
    title = md("**Listing Of Doctor Who Episodes**"),
    subtitle = md("Top 10 Directors By  Viewers")) |> 
  tab_source_note("Source: TidyTuesday | week48.") |>
  tab_source_note(source_note = md("Birusod, 2023")) |> 
  tab_spanner(
    label = "CHARACTERISTICS",
    columns = c(VIEWERS, RATING, DURATION)) |> 
  cols_label(
    VIEWERS = html("Total viewers<br>in UK"),
    RATING = html("Rating<br>by fans"),
    DURATION = html("Duration<br>in minutes")) |> 
  #cols_width(VIEWERS ~px(150)) |> 
  cols_align(align = "center", 
             columns = c(VIEWERS, RATING, DURATION))  |>  
  tab_options(column_labels.font.weight = "bold")  |> 
  tab_style(
    style = cell_text(
      align = "center", color = 'grey50',
      size = "smaller", weight = "lighter", style = "italic",
      transform = "uppercase"),
    locations = cells_column_labels(
      columns = c(VIEWERS, RATING, DURATION))) |> 
  fmt_number(columns = VIEWERS, 
             suffixing = FALSE,
             sep_mark = ',',
             drop_trailing_zeros = TRUE) |> 
  fmt_date(
    columns = DATE,
    date_style = "m_day_year") |> 
  opt_align_table_header(align = "center") |> 
  opt_table_font(
    font = list('serif')) |> 
   tab_style(
    style = list(
      cell_fill(color = "#F9E"),
      cell_text(weight = "bold")),
    locations = cells_body(
      #columns = DURATION,
      rows = DURATION == max(DURATION))) |> 
  tab_footnote(
    footnote = "Deep Breath was the longuest episode",
    placement = 'auto',
    locations = cells_body(
      columns = DURATION,
      rows = DURATION == max(DURATION))) |> 
  opt_footnote_marks(marks = "standard")


####  Percent bars  ----
writers_tab |> 
  select(-c(DATE, DURATION)) |> 
  mutate(PCT = VIEWERS/max(VIEWERS) * 100,
         RTG = RATING) |> 
  arrange(desc(VIEWERS)) |> head(10) |> 
  gt()  |> 
  tab_header(
    title = md("**Listing Of Doctor Who Episodes**"),
    subtitle = md("Top 10 Writers By  Viewers")) |> 
  tab_source_note("Source: TidyTuesday | week48.") |>
  tab_source_note(source_note = md("Birusod, 2023")) |> 
  #gtExtras
  gt_plt_bar(column = RTG, keep_column = FALSE) |> 
  gt_plt_bar_pct(column = PCT, 
                 fill = "blue", background = "lightblue") |> 
  cols_align("center", columns = c(RTG, contains("PC"))) |> 
  cols_width(1 ~ px(200),
             2 ~ px(250),
             5 ~ px(150),
             6 ~ px(150))

#### Stacked percent  -----
fake_data |> 
  group_by(director) |> 
  summarise(list_data = list(rating)) |> 
  gt() |> 
  gt_plt_bar_stack(
    list_data, 
    width = 65,
    labels = c(" LOW ", " MED ", " HIGH "),
    palette = c("#ff4343", "grey", "#2D8077")) |> 
  #gt_theme_538()
  #gt_theme_dark()
  gt_theme_nytimes()


#### Bullet chart  -----
success_df |> 
  ungroup() |> 
  gt() |> 
  gt_plt_bullet(
    column = Success, target = Avg, width = 45,
    palette = c("#BAD8EA", "#d62728")) |> 
  fmt_number(columns = Viewers, 
             suffixing = FALSE,
             sep_mark = ',',
             drop_trailing_zeros = TRUE) |> 
  tab_header(
    title = md("**Listing Of Doctor Who Episodes**"),
    subtitle = md("Top Rated Directors")) |> 
  tab_options(column_labels.font.weight = "bold")


## gtsummary ----
hist(df$viewers)

theme_gtsummary_compact()
reset_gtsummary_theme()
df |> 
  mutate(popularity = case_when(
    viewers <= 6 ~ 'Low',
    viewers <= 8 ~ 'Medium',
    TRUE ~ 'High')) |> 
  select(pos, viewers, rating, duration, popularity) |> 
  mutate(pos = fct_relevel(pos, 'Low', 'High')) |> 
  tbl_summary(
    by = popularity) |> 
  add_n() |> 
  add_p() |> 
  modify_header(
    label = "**Variable**",
    all_stat_cols() ~ "**{level}**, N={n} ({style_percent(p)}%)") |>
  bold_labels() |> 
  italicize_levels()


## Huxtable  -------
hxtab <- success_df |> 
  as_hux() |> 
  set_all_padding(1:11, 1:6, 4) |> 
  set_outer_padding(1:11, 1:6, 0) |> 
  set_number_format(1:11, 1:6, 0) |> 
  set_bold(row = 1, col = everywhere) |> 
  set_bottom_border(row = 1, col = everywhere) |> 
  set_width(.6) |> 
  set_caption("Listing Of Doctor Who Episodes")

hxtab |> 
  set_col_width(c(.4, .3, .1, .1, .1, .1)) |> 
  theme_striped() |> # theme_grey
  as_flextable()

hxtab |> 
  set_col_width(c(.4, .3, .1, .1, .1, .1)) |> 
  theme_compact() |> # theme_bright() theme_green()
  as_flextable()
  

hxtab |> 
  set_col_width(c(.4, .3, .1, .1, .1, .1)) |> 
  set_background_color(evens, everywhere, "grey80") |> 
  set_background_color(odds, everywhere, "grey90") |> 
  set_text_color(everywhere, 6, "red") |> 
  set_right_border(everywhere, 1, brdr(3, "double", "blue")) |> 
  set_header_rows(1, TRUE) |> 
  set_header_cols(1, TRUE) |> 
  style_header_rows(bold = TRUE, text_color = "grey30") |> 
  style_header_cols(bold = TRUE, text_color = "dodgerblue") |> 
  as_flextable()

## flextable  ----
# https://ardata-fr.github.io/flextable-book/static/pdf/cheat_sheet_flextable.pdf
#set_flextable_defaults(
#  font.size = 10, theme_fun = theme_vanilla,
#  padding = 6, background.color = "#EFEFEF")

### flextable themes -----
ftm |> flextable()
ftm |> flextable() |> theme_alafoli()
ftm |> flextable() |> theme_apa()
ftm |> flextable() |> theme_booktabs() #default
ftm |> flextable() |> theme_vanilla()
ftm |> flextable() |> theme_box()
ftm |> flextable() |> theme_tron()
ftm |> flextable() |> theme_tron_legacy()
ftm |> flextable() |> theme_vader()
ftm |> flextable() |> theme_zebra()


### flextable formatting -----
fx1 <- ftm |> flextable()
fx2 <- ftm |> flextable() |> theme_tron()

fx1 |> 
  theme_vanilla() |> 
  add_header_row(colwidths = c(1, 2, 3, 1),
                 values = c("", "Time", "Measure", "")) |>
  color(part = "footer",  color = "#666666") |> 
  color(part = "header",  color = "#AA0000") |> 
  color(part = "body",  color = "#17becf", 1:12, 1) |> 
  set_header_labels(pos = "position", director  = "Director",
                    date = "Date", season = "Season",
                    viewers = "Viewers", rating = "Rating", 
                    duration = "Duration") |> 
  align(align = "center", part = "header", 1:2, 2:7) |> 
  align(align = "center", part = "body", 1:12, 2:7) |> 
  set_table_properties(layout = "autofit", width = .8) |> 
  border_inner_v(part = "body") |> 
  autofit() |> 
  set_caption(
    as_paragraph(
      as_chunk(
        "Dr Who: Selected Directors", 
        props = fp_text_default(
          font.family = "Arial",
          color = "#476BA0",
          font.size = 20,
          bold =  TRUE)))) |> 
  add_footer_lines(
    as_paragraph(
      as_chunk("Dr Who | TidyTuesday | Week 48",
               props = fp_text_default(
                 font.family = "Arial",
                 color = "grey",
                 font.size = 10,
                 italic =  TRUE)))) |> 
  align( align = "right", part = "footer")
  

  
### flextable with bars ------

ftm |> 
  select(-pos) |> 
  flextable() |> 
  theme_tron() |> 
  autofit() |> 
  mk_par(
    j = 6,
    value = as_paragraph(
         minibar(value = duration, 
                 max = max(duration), 
                 height = .2,
                 barcol = "#17becf",
                 bg = "grey90")), # transparent
       part = "body") |> 
  colformat_date(fmt_date = "%d %b %Y") |>
  mk_par(
    part = "header", j = "viewers",
    value = as_paragraph("viewers", as_i(as_sub("mlns")))
  )


## kableExtra & formattable -----
w15 <- 
  writers_tab |> 
  arrange(desc(VIEWERS)) |> 
  filter(!duplicated(WRITER)) |> 
  head(15)
kb <- w15 |> 
  kbl(caption = "Listing Of Doctor Who Episodes")
kb

kb |> kable_styling()
kb |> kable_classic()
kb |> kable_classic_2()
kb |> kable_material()
kb |> kable_material_dark()
kb |> kable_minimal(html_font = "arial")

kb |> 
  kable_paper(
    html_font  = 'roboto',
    "hover",
    full_width = F)
wrtrs <- "https://en.wikipedia.org/wiki/List_of_Doctor_Who_writers"
kb |> 
  kable_styling(
    html_font  = 'roboto',
    bootstrap_options = c(
      "striped",  "hover" ,
      "condensed", "responsive", "bordered"),
    full_width = F, 
    font_size = 14) |> 
  column_spec(1, 
              bold =  TRUE,
              color = spec_color(w15$RATING),
              link = wrtrs,
              popover = spec_popover(
                title = "Episode writer",
                content = "Example of popup message."
              ))  |> 
  column_spec(6,
              color = "red",
              background = spec_color(
                w15$RATING,
                option = "C", 
                direction = -1)) |> 
  column_spec(5, width = "5em", 
              color = "crimson", bold = T,
              background = "wheat",
              tooltip = paste0(
                "Writer: ", w15$WRITER)) |> 
  kable_classic("striped", full_width = F) |> 
  scroll_box(height = "400px")


## Reactable -------

### Basic reactible   ----
rtb |> reactable()

rtb1 <- rtb |> 
  reactable(
    defaultSorted = "Viewers",
    columns = list(
      Date = colDef(
        name = "Date aired",
        format = colFormat(date = TRUE)),  # prefix = "@"
      Viewers = colDef(
        name = "Viewers (Mlns)",
        defaultSortOrder = "desc",
        format = colFormat(separators = TRUE,
                           percent = FALSE, 
                           digits = 2)),
      Rating = colDef(
        name = "Exclusive Followers",
        defaultSortOrder = "desc")
    )
  )
### reactible formatted  ----
# Set the default theme for all tables
options(
  reactable.theme = reactableTheme(
    color = "hsl(233, 9%, 87%)",
    backgroundColor = "hsl(233, 9%, 19%)",
    borderColor = "hsl(233, 9%, 22%)",
    stripedColor = "hsl(233, 12%, 22%)",
    highlightColor = "hsl(233, 12%, 24%)",
    inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonHoverStyle = list(
      backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonActiveStyle = list(
      backgroundColor = "hsl(233, 9%, 28%)")))
# Reset to default theme
rtb |> reactable()
options(reactable.theme = reactableTheme())  # empty style to reset default
rtb |> reactable()
rtb |> 
  reactable(
    filterable = TRUE,
    showPageSizeOptions = TRUE,
    striped = TRUE, highlight = TRUE, compact = TRUE,
    details = function(index) paste("Details for row", index),
    defaultColDef = colDef(minWidth = 100),
    columns = list(
      Director = colDef(
        width = 150,     # width = "20%"
        sticky = "left", # Add a left border style 
        style = list(borderRight = "2px solid #B2ADA3",
                     fontWeight = 600, color = '#1f77b4' ),
        headerStyle = list(borderRight = "1px solid #eee")),
      Rating = colDef(
          style = function(value) {
            if (value > mean(rtb$Rating)) {
              color <- "#008000"
            } else if (value < mean(rtb$Rating)) {
              color <- "#e00000"
            } else {
              color <- "#777"
            }
            list(color = color, fontWeight = "bold")
          }
        ))
  )

### Formatting with reactablefmtr  -----

library(htmltools)
bar_chart <- function(label, width = "100%", 
                      height = "16px", fill = "#15607A", 
                      background = "#EEEEEE") {
  bar <- div(style = list(background = fill, 
                          width = width, 
                          height = height))
  chart <- div(style = list(flexGrow = 1, 
                            marginLeft = "8px", 
                            background = background), 
               bar)
  div(style = list(display = "flex", 
                   alignItems = "center"), 
      label, 
      chart)
}
rtb |> 
  reactable(
    striped = TRUE, highlight = TRUE, compact = TRUE,
    searchable = TRUE, resizable = TRUE,
    defaultSorted = "Viewers",
    defaultColDef = colDef(minWidth = 70),
    theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#e9d8a6",
      highlightColor = "#ff7f0e"),
    defaultPageSize = 10,
    columns = list(
      Director = colDef(width = 150),
      Duration = colDef(
        align = "left", 
        cell = function(value) {
          width <- paste0(value / max(rtb$Duration) * 100, "%")
          bar_chart(value, width = width)
        }))) |> 
  add_source(
    source = 'Dr Who | TidyTuesday | Week 48',
    font_style = 'italic',
    font_weight = 'normal',
    font_color  = "grey80",
    font_size = 12) |> 
  add_title(
    title = 'Dr Who: Selected Directors',
    font_color = '#666666', font_size = 16,
    margin = reactablefmtr::margin(t = 0, b = 0)) |> 
  add_subtitle(
    subtitle = 'Sorted By Total Viewers (in Millions)',
    font_size = 14,
    font_color = '#666666',
    margin = reactablefmtr::margin(t = 0, b = 0)) 


final_table <- rtb %>% 
  reactable(.,
    theme = fivethirtyeight(centered = TRUE, header_font_size = 11),
    defaultSorted = "Viewers",
    columns = list(
      Director = colDef(maxWidth = 250),
      Duration = colDef(
        maxWidth = 180,
        cell = data_bars(., text_size = 13, box_shadow = TRUE)
        ),
      Rating = colDef(
        maxWidth = 180,
        align = "center",
        cell = icon_sets(., 
                         icon_set = "batteries", 
                         icon_position = "over"),
        style = list(borderRight = "1px solid #777")))
    ) %>% 
  add_title(
    "Dr Who: Selected Directors", 
    font_color = '#005f73', font_size = 20,
    margin = margin(0, 0, 10, 0)) %>% 
  add_subtitle(
   subtitle = 'Sorted By Total Viewers (in Millions)',
   font_size = 14,
   font_color = '#666666',
   margin = reactablefmtr::margin(t = 0, b = 0)) %>%
  add_icon_legend(icon_set = "batteries") %>% 
  add_source("Dr Who | TidyTuesday | Week 48", 
             font_style = 'italic',
             font_weight = 'normal',
             font_color  = "grey80",
             font_size = 12)

  
# Saving Plots and Gifs ------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-11-28", paste0("20231128", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )

dir_to_save
getwd()
final_table  %>%
  reactablefmtr::save_reactable_test("/2023/w48_Doctor_Who/plots_w48/final_table.png")
