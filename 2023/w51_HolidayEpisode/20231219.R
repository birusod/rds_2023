
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, ggthemes,
  showtext, patchwork, ggtext, glue,
  ggstream, wesanderson, colorspace,
  tm, wordcloud, SnowballC, tokenizers, tidytext  # for text  analysis
  )


# Loading data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-12-19")

# raw data overview
tuesdata$holiday_episodes |> head() |> view()
tuesdata$holiday_episode_genres |> head() |> view()

# assessing missing values
tuesdata$holiday_episodes |> 
  summarise_all(list(~ round(mean((is.na(.))), 2))) |> 
  pivot_longer(everything(), names_to = 'vars', values_to = 'pct') |> 
  filter(pct > 0)


# Loading fonts ------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Fira code", "firac")
font_add_google("Fira sans", "firas")
font_add_google("Montserrat", "montserrat")
font_add_google("Great Vibes", "gvibes")
font_add_google("Ultra", "ultra")
font_add_google("Festive", "festive")
font_add_google("Pinyon Script", "pinyon")
font_add_google("Pacifico", "pacifico")
showtext_auto()


# Defining colors ---------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""

pal1 <- wes_palette("Darjeeling1")
pal2 <- darken(pal1, 0.2)
pal3 <- c("#FFB400", "#9467bd", "#FF020D", "#13AFEF", "#FFC740")
pal <- c(rbind(pal1, pal2))

bg <- "#555665"

gr1 <- "grey85"
gr2 <- "grey97"
gr3 <- "#B2ADA3"


# Wrangling Data ----------------------------------------------------------

## clean data -------
episodes <- tuesdata$holiday_episodes |> # head(20) |> view()
  replace_na(list(year = 1997)) |> 
  select(
    tconst, year, 
    genre = genres,
    type = parent_title_type,
    runtime = runtime_minutes,
    rating = average_rating,
    votes = num_votes,
    christmas, hanukkah, kwanzaa, holiday
  ) |> 
  pivot_longer(cols = c(christmas, hanukkah, kwanzaa, holiday),
               names_to = 'holiday',
               values_to = 'celebrated') |> 
  mutate(decade = year %/% 10 * 10)

tvseries <- tuesdata$holiday_episodes |> 
  select(
    start_year = parent_start_year,
    end_year = parent_end_year,
    genre =  parent_genres,
    rating = parent_average_rating,
    runtime = parent_runtime_minutes,
    votes = parent_num_votes
  )

genre_df <- tuesdata$holiday_episode_genres |> rename(genre = genres)

titles_df <- tuesdata$holiday_episodes |> 
  select(tconst, year, genres, title = simple_title)



## By genres top20 -----
top20_genres <- genre_df |> 
  drop_na() |> 
  mutate(genre = str_to_upper(fct_lump_n(genre, n = 19))) |> 
  count(genre, sort = TRUE)



## Holiday reference by decade ------

hday_decades <- episodes |> 
  mutate(decade = factor(decade)) |> 
  group_by(decade, holiday) |> 
  summarise(total = sum(celebrated)) |> 
  mutate(holiday = str_to_upper(holiday))
hday_decades_smr <- episodes |> 
  mutate(decade = factor(decade)) |> 
  group_by(decade, holiday) |> 
  summarise(avg = mean(votes),  .groups = 'drop') |> 
  mutate(holiday = str_to_upper(holiday))
  

## Yearly Holiday reference ------

hday_years <- episodes |> 
  group_by(year, holiday) |> 
  count(name = 'total') |> 
  ungroup() |> 
  mutate(holiday = str_to_upper(holiday))

hday_years_smr <- episodes |> 
  group_by(year, holiday) |> 
  summarise(avg = mean(votes),  .groups = 'drop') |> 
  mutate(holiday = str_to_upper(holiday))
  

hday_years2 <- episodes |> 
  count(year, holiday, name = 'total') |> 
  mutate(holiday = str_to_upper(holiday)) |> 
  group_by(year, holiday) |> 
  mutate(avg = mean(total)) |> 
  ungroup() |> 
  pivot_longer(c(total, avg), names_to = 'metric', values_to = 'total')



## Text mining ------
tdf <- titles_df |>  
  select(-genres) |> 
  left_join(genre_df, by  = join_by(tconst)) |> 
  left_join(
    episodes |> 
      filter(celebrated == TRUE) |> 
      select(tconst, holiday),
    by  = join_by(tconst),
    relationship = "many-to-many")

# tidyetxt

tdf_tx <- tdf |> 
  unnest_tokens("Word", "title") |> 
  anti_join(stop_words, by = c("Word" = "word")) |> 
  filter(!str_detect(Word, '[0-9]'))


# Wordcloud
wdir <- "/Users/birusod/Documents/DataScienceDocs/GitProjects/R4DS/rds_2023/2023/w51_HolidayEpisode/"
wc <- tdf |> pull(title) |> as.list() 
data.table::fwrite(wc,paste(wdir, "test.txt"), 
                   sep = ";",col.names = FALSE, row.names = FALSE)


mooncloud <- Corpus(DirSource(directory = wdir, pattern = 'txt'))
mooncloud |> inspect()
wcc <- mooncloud |>
  tm_map(stripWhitespace) |>                   # Strip unnecessary whitespace
  tm_map(tolower) |>                           # Convert to lowercase
  tm_map(removeWords, stopwords("english")) |> # Remove conjunctions etc.
  tm_map(stemDocument) |>           # suffixes to the common 'stem'
  tm_map(removePunctuation) |>      # Remove commas etc.
  tm_map(PlainTextDocument)         # #(optional) avoid conversion


# Data Viz -------------------------------------------------------------------

## Top genres:  ------
top20_genres |>
  ggplot(aes(n,  fct_reorder(genre, n))) +
  geom_col()  +
  labs(title = 'Episodes Distribution By Genre',
       subtitle = 'Top 20 Most Frequent Genre',
       x = NULL, y = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_light() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )




## Holiday reference ------

hday_decades |> 
  ggplot(aes(decade, total, fill = holiday)) +
  geom_col()

hday_decades_smr |> 
  ggplot(aes(decade, avg, fill = holiday)) +
  geom_col()

hday_years |> 
  ggplot(aes(year, total, fill = holiday)) +
  geom_col()
hday_years_smr |> 
  ggplot(aes(year, avg, fill = holiday)) +
  geom_col()

# https://github.com/gkaramanis 2021-week11
hday_decades_smr |> 
  ggplot(aes(decade, avg, fill = holiday)) +
  geom_stream(bw = 1)

hday_years_smr |> 
  ggplot(aes(year, avg, fill = holiday)) +
  geom_stream()

# ggplot2::annotate error caused by loading NLP
hday_years2 |> 
  ggplot(aes(year, total, 
             fill = interaction(desc(metric), holiday),
             label =  if_else(metric == "avg", holiday, ""))) +
  geom_stream(bw = .4) +
  geom_stream_label(family = 'roboto', size = 6, color = gr2) +
  ggplot2::annotate("text", x = 1950, y = 200, label = "HOLIDAYS\nEPISODES", 
           hjust = 0, size = 9, color = gr1, 
           family = 'roboto', lineheight = 0.8) +
  ggplot2::annotate("text", x = 1950, y = -300, 
           label = "TidyTuesday | week51\nGraphic: Inspired by  G. Karamanis", 
           size = 3, color = gr1, hjust = 0, 
           family = 'montserrat', lineheight = 0.9) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()), 
    breaks = breaks_width(200), 
    position = "right") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg, color = NA),
    panel.grid.major.x = element_line(size = 0.5, color = lighten(bg, 0.1)),
    axis.text.x = element_text(
      family = 'montserrat', color = lighten(bg, 0.8), 
      margin = margin(10, 0, 0, 0), size = 14),
    axis.text.y = element_text(
      family = 'montserrat', color = lighten(bg, 0.8), 
      size = 12, hjust = 0),
    plot.margin = margin(20, 20, 20, 20)
  )


hday_years |> 
  ggplot(aes(year, total, 
             fill = holiday,
             label =  holiday)) +
  geom_stream(bw = .7, color = 1, lwd = 0.25) +
  #geom_stream_label(family = 'roboto', size = 4, color = gr2) +
  #annotate("text", x = 1950, y = 200, label = "HOLIDAYS EPISODES\nBY TYPE", 
  #         hjust = 0, size = 9, color = gr1,  family = 'pacifico') +
  ggplot2::annotate("text", x = 1995, y = 150, label = "CHRISTMAS", 
           hjust = 0, size = 5, color = pal3[1],  family = 'ultra') +
  ggplot2::annotate("text", x = 1980, y = 100, label = "HANUKKAH", 
           hjust = 0, size = 5, color = pal3[2],  family = 'ultra') +
  ggplot2::annotate("text", x = 1980, y = -100, label = "HOLIDAY", 
           hjust = 0, size = 5, color = pal3[3],  family = 'ultra') +
  ggplot2::annotate("text", x = 1995, y = -150, label = "KWANZAA", 
           hjust = 0, size = 5, color = pal3[4],  family = 'ultra') +
  labs(caption = "TidyTuesday | week51\nGraphic: Inspired by  G. Karamanis",
       title = "HOLIDAYS EPISODES",
       subtitle = 'BY TYPE') +
  scale_fill_manual(values = pal3) +
  scale_y_continuous(
    labels = c('+200', '+100', '0', '+100', '+200'), 
    # label_number(scale_cut = cut_short_scale())
    breaks = c(seq(-200, 200, 100)),             #breaks_width(100), 
    position = "right") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = 'black', color = NA),
    panel.grid.major.x = element_line(size = 0.2, color = lighten(bg, 0.1)),
    axis.text.x = element_text(
      family = 'montserrat', color = lighten(bg, 0.8), 
      margin = margin(10, 0, 0, 0), size = 14),
    axis.text.y = element_text(
      family = 'montserrat', color = lighten(bg, 0.8), 
      size = 12, hjust = 0),
    plot.title = element_text(hjust = 0.5, size = 30, color = gr1,  
                              family = 'pacifico'),
    plot.subtitle = element_text(hjust = 0.5, size = 16, color = gr3,  
                              family = 'pacifico'),
    plot.caption = element_text(
      size = 8, color = gr1, hjust = 1, 
      family = 'montserrat', lineheight = 0.9),
    plot.margin = margin(40, 20, 20, 20)
  )


hday_years_smr |> 
  ggplot(aes(year, avg, 
             fill = holiday,
             label =  holiday)) +
  geom_stream(bw = .7, color = 1, lwd = 0.25) +
  geom_stream_label(family = 'ultra', size = 4, color = gr2) +
  labs(caption = "TidyTuesday | week51",
       title = "HOLIDAYS EPISODES",
       subtitle = 'Average Votes By Type') +
  scale_fill_manual(values = pal3) +
  scale_y_continuous(
    labels = c('+500', '+250', '0', '+250', '+500'), 
    #label_number(scale_cut = cut_short_scale())
    breaks = c(seq(-500, 500, 250)),             #breaks_width(100), 
    position = "right") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = 'black', color = NA),
    panel.grid.major.x = element_line(size = 0.2, color = lighten(bg, 0.1)),
    axis.text.x = element_text(
      family = 'montserrat', color = lighten(bg, 0.8), 
      margin = margin(10, 0, 0, 0), size = 14),
    axis.text.y = element_text(
      family = 'montserrat', color = lighten(bg, 0.8), 
      size = 12, hjust = 0),
    plot.title = element_text(hjust = 0.5, size = 30, color = gr1,  
                              family = 'pacifico'),
    plot.subtitle = element_text(hjust = 0.5, size = 16, color = gr3,  
                                 family = 'pacifico'),
    plot.caption = element_text(
      size = 8, color = gr1, hjust = 1, 
      family = 'montserrat', lineheight = 0.9),
    plot.margin = margin(40, 20, 20, 20)
  )



hday_years_smr |> 
  ggplot(aes(year, avg, 
             fill = holiday,
             label =  holiday)) +
  geom_stream(bw = .7, color = 1, lwd = 0.25, type = 'ridge') +
  #geom_stream_label(family = 'ultra', size = 4, color = gr2) +
  labs(caption = "TidyTuesday | week51",
       title = "HOLIDAYS EPISODES",
       subtitle = 'Average Votes By Type From 1949 To 2023',
       fill = "") +
  scale_fill_manual(values = pal3) +
  scale_y_continuous(position = "right") +
  theme_void() +
  guides(fill = guide_legend(
    label.position = 'top',
    keyheight = .5,
    keywidth = 8,
    label.hjust = 1
  )) +
  theme(
    legend.position = c(0.1, .5),
    legend.text = element_text(color = gr2),
    plot.background = element_rect(fill = 'black', color = NA),
    panel.grid.major.x = element_line(size = 0.2, color = lighten(bg, 0.1)),
    axis.text.x = element_text(
      family = 'montserrat', color = lighten(bg, 0.8), 
      margin = margin(10, 0, 0, 0), size = 14),
    axis.text.y = element_text(
      family = 'montserrat', color = lighten(bg, 0.8), 
      size = 12, hjust = 0),
    plot.title = element_text(hjust = 0.5, size = 30, color = gr1,  
                              family = 'pacifico'),
    plot.subtitle = element_text(hjust = 0.5, size = 16, color = gr3,  
                                 family = 'pacifico'),
    plot.caption = element_text(
      size = 8, color = gr1, hjust = 1, 
      family = 'montserrat', lineheight = 0.9),
    plot.margin = margin(40, 20, 20, 20)
  )

dx <- data.frame(
  x = rep(1:10, 3), 
  y = sample.int(100, 30), 
  group = sort(rep(c("A", "B", "C"), 10))) |> 
  group_by(group) |> 
  mutate(y2  = y/sum(y))
ds <- dx |> 
  ggplot(aes(x, y, fill = group, label = group)) +
  geom_stream_label(n_grid = 100)
ds  + geom_stream() + labs(title = 'TYPE: mirror (default')
ds  + geom_stream(type = 'ridge') + labs(title = 'TYPE: ridge')
ds  + geom_stream(type = 'proportional') + 
  labs(title = 'TYPE: proportional')
  

## Text analysis viz  -------

# custom func
words_freq_plot <- function(df, col, top = 10){
  df |>
    count({{col}}, sort = TRUE) |>
    mutate({{col}} := factor({{col}}, levels = rev(unique({{col}})))) |>
    slice_head(n = top) |>
    ungroup() |>
    ggplot(mapping = aes(x = n, y = {{col}})) +
    geom_col(show.legend = FALSE) +
    theme_light() +
    labs(x = NULL)
}

tdf_tx |> words_freq_plot(Word)
tdf_tx |> words_freq_plot(Word, 15)


## Wordcloud ------
wcc2 <- tdf_tx |> 
  filter(Word != 'christmas') |> 
  sample_frac(.2) |> 
  pull(Word) 

wcc2 |> 
  VectorSource() |>
  VCorpus() |> 
  tm_map(content_transformer(tolower)) |>
  wordcloud(scale = c(5,0.5),
            max.words = 100,
            random.order = FALSE,
            rot.per = 0.35,
            use.r.layout = FALSE,
            colors = MetBrewer::met.brewer(name = "Signac")[3:14]
            ) 

pacman::p_load(wordcloud2)
wcc3 <- tdf_tx |> 
  filter(Word != 'christmas') |> 
  sample_frac(.1) |> 
  count(Word, sort = TRUE) |> 
  rename(word = 1, freq = 2) 
wcc3 |>
  wordcloud2(
    shape = 'cardioid',
    size = 1.6, 
    color = 'random-light', 
    backgroundColor = "black")

wcc4 <- tdf_tx |> 
  filter(Word != 'christmas') |> 
  sample_frac(.1) |> 
  count(Word, sort = TRUE) |> 
  rename(word = 1, freq = 2) 
wcc4 |>
  wordcloud2(
    size = 1.6, 
    color = MetBrewer::met.brewer(name = "Redon")[1:12])

# Saving Plots and Gifs ------------------------------------------------------

# gg_playback(
#   name = file.path("2023", "2023-12-19", paste0("20231219", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
