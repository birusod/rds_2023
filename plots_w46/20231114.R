
# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, 
  showtext, patchwork, ggtext, glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-11-14")
ds <- tuesdata$diwali_sales_data |> 
  clean_names() |> 
  mutate(marital_status = case_when(
    marital_status == 1 ~ 'Single',
    TRUE ~ 'Married')) |> 
  mutate(
    state = fct_rev(fct_infreq(state)),
    zone = fct_rev(fct_infreq(zone)),
    occupation = fct_rev(fct_infreq(occupation)),
    category = fct_rev(fct_infreq(product_category))) |> 
  select(-c(cust_name, product_category))


sq <- c(1,2,3,4,5,3,1)
sq[duplicated(sq)]
tib <- tibble(sq = sq)
tib |> filter(!duplicated(sq))


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Eater", "eater")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()


# Define colours ----------------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""


# Data wrangling ----------------------------------------------------------
ds |> head() |> view()
ds |> glimpse()


# EDA ---------------------------------------------------------------------
ds |> colnames()
ds |> count(age_group)
ds |> count(orders)

ds |> count(zone)
ds |> count(occupation)
ds |> count(gender)
ds |> count(marital_status)
ds |> count(product_category)
ds |> count(state)

hist(ds$amount)
hist(ds$age)

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

ds |> 
  group_by(gender) |> 
  count(marital_status) |> 
  ggplot(aes(marital_status, n, fill = gender)) +
  geom_col(position = 'dodge')


# bar plot function -------
by_gender_plot <- function(data, gvar, cvar, ptitle, sub, legend, flip = F){
  p <- data |> 
    group_by({{gvar}}) |> 
    count({{cvar}}) |> 
    ggplot(aes({{cvar}}, n, fill = {{gvar}})) +
    geom_col(position = 'dodge')  +
    labs(y = NULL, x = NULL, 
         title = ptitle, subtitle = sub,
         fill = legend) +
    scale_y_continuous(
      expand = c(0, 0),
      labels = comma) +
    scale_fill_brewer(palette = 'Pastel1') +
    theme_minimal(base_size = 12, base_family = 'roboto') +
    theme(
      plot.title = element_text(face = 'bold', size = 18),
      plot.subtitle = element_text(
        face = 'bold', size = 14, color = 'firebrick3'),
      axis.text = element_text(face = 'bold'),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.title = element_text(face = 'bold'),
      legend.text = element_text(face = 'bold'),
      legend.position = 'top'
    )
  if (flip) {
    p + coord_flip()
  }else{
    p
  }
}

ds |> by_gender_plot(gender, marital_status,
                 ptitle = 'Gender Distribution', 
                 sub = 'By marital status',
                 legend = 'Gender')

ds |> by_gender_plot(gender, age_group,
                 ptitle = 'Gender Distribution', 
                 sub = 'By age group',
                 legend = 'Gender')
ds |> by_gender_plot(gender, zone,
                 ptitle = 'Gender Distribution', 
                 sub = 'By zone',
                 legend = 'Gender')

ds |> by_gender_plot(gender, state, flip = T,
                 ptitle = 'Gender Distribution', 
                 sub = 'By state',
                 legend = 'Gender')

ds |> by_gender_plot(gender, category, flip = T,
                 ptitle = 'Gender Distribution', 
                 sub = 'By category',
                 legend = 'Gender')

ds |> by_gender_plot(gender, occupation, flip = T,
                 ptitle = 'Gender Distribution', 
                 sub = 'By age group',
                 legend = 'Gender')

# Plotting library: highchartr -----------------------------
# https://jkunst.com/highcharter/index.html
# https://www.datanovia.com/en/lessons/highchart-interactive-bar-plot-in-r/
# Book: https://rpubs.com/zac-garland/highcharter-chartbook
# geek: https://www.geeksforgeeks.org/how-to-plot-timeseries-using-highcharter-library-in-r/

library(highcharter)
library(jsonlite)

# sankey  charts ----------
select(diamonds, cut, color, clarity) |> 
  data_to_sankey()
sank <- ds |> 
  count(gender, age_group) |> 
  rename(from = gender,
         to = age_group,
         weight = n) |> 
  toJSON() 

highchart() |> 
  hc_chart(type = 'sankey') |> 
  hc_add_series(data = sank) |> 
  hc_title(
    text = '<b>BUYERS AGE & GENDER</b>',
    style = list(color = "firebrick", 
                 fontFamily = "eater", 
                 fontSize = "26px")
  )

ds |> 
  select(age_group, gender, zone) |> 
  data_to_sankey() |> 
  hchart("sankey")
  

# dependency wheel
highchart() |> 
  hc_add_series(
    data = sank,
    type = 'dependencywheel') |> 
  hc_title(
    text = '<b>BUYERS<br>AGE & GENDER</b>',
    style = list(color = "firebrick", 
                 fontFamily = "eater", 
                 fontSize = "30px")
  )


# hc bar chart --------------

zone_bar_data <- ds |> 
  count(zone, name = 'total') |> 
  arrange(desc(total))
zone_bar_data |> 
  hchart('bar', 
         hcaes(x = zone, y = total),
         color = "lightgray", borderColor = "black") |> 
  hc_tooltip(sort = TRUE, table = TRUE) |> 
  hc_tooltip(pointFormat = "<br>{point.y}")  |> 
  hc_title(text = "Frequency Of Buyers By Zone") |>
  hc_add_theme(hc_theme_tufte())

mcols <- c("#0073C2FF", "#EFC000FF", "#603C14", "#CD4F41", "#2D8077")
zone_bar_data |> 
  hchart(
    'column', 
    hcaes(x = zone,  color = mcols,y = total))  |> 
  hc_yAxis(title = list(text = "Frequency"))  |> 
  hc_xAxis(title = "", 
           labels = list(
             style = list(fontWeight = "bold", 
                          fontSize = "16px",
                          color = '#B46A93'))) |> 
  hc_title(
    text = "<b>DISTRIBUTION OF BUYERS</b>", align = "center",
    style = list(color = "#22A884", fontFamily = "Roboto", fontSize = "25px")) |>
  hc_subtitle(
    text = "<b><i>By Zone</i></b>", align = "center",
    style = list(color = "#CD1F83", fontFamily = "Roboto", fontSize = "18px")) |> 
  hc_caption(text = "Diwali Sales Data") |> 
  hc_tooltip(crosshairs = FALSE,
             pointFormat = "<b>{point.y}</b>")

# More bar charts  --------
# quick function to plot selected categorical variables:

plot_bar_chart <- function(data, grpvar, subtitle, color){
  dat <- data |> 
    count({{grpvar}}, name =  "total") |> 
    arrange(desc(total)) |> 
    mutate(grpcolor := color)
  dat |> 
    hchart('bar', 
           hcaes(x = !!as.name(subtitle), y = total, color = grpcolor))  |> 
    hc_yAxis(
      title = list(text = "Frequency"),
      labels = list(
        style = list(fontWeight = "bold", 
                     fontSize = "12px"))) |> 
    hc_xAxis(
      title = list(text = ""),
      labels = list(
        style = list(
          fontWeight = "bold", fontSize = "12px",
          color = color))) |> 
    hc_title(
      text = "<b><p style='color:black;font-family:Roboto; font-size:25px'>Frequency Of Buyers</p></b>",
      style = list(fontSize = "20px")) |> 
    hc_subtitle(
      text = paste("By ", subtitle),
      style = list(fontFamily = "eater",  fontSize = "20px",
                   fontWeight = "bold", color = color)) |> 
    hc_tooltip(headerFormat = "",
               pointFormat = "<b>Total: {point.y}</b>")
}
create_group_count <- function(data, grpvar, color){
  data |> 
    count({{grpvar}}, name =  "total") |> 
    arrange(desc(total)) |> 
    mutate(grpcolor := color)
} # not needed
create_group_count(ds, occupation, mcols[1])

ds |> plot_bar_chart(
  grpvar = occupation, 
  subtitle = "occupation", 
  color = mcols[1])

ds |> plot_bar_chart(
  grpvar = category, 
  subtitle = "category", 
  color = mcols[4])

ds |> plot_bar_chart(
  grpvar = state, 
  subtitle = "state", 
  color = mcols[5])
 
# Age group by gender ----------- 
ds |> 
  count(age_group, marital_status) |> 
  hchart(
    'column', 
    hcaes(
      x = age_group, 
      group = marital_status,
      y = n)) |> 
  hc_colors(c("#0073C2FF", "#EFC000FF")) |> 
  hc_yAxis(title = list(text = "Frequency"))  |> 
  hc_xAxis(title = list(text = "Age groups"), 
           labels = list(
             style = list(fontWeight = "bold", 
                          fontSize = "16px"))) |> 
  hc_title(text = 'Marital Status By Age Group',
           style = list(color = "darkgreen", 
                        fontFamily = "ubuntu", 
                        fontSize = "30px")) |> 
  hc_legend(layout = "horizontal",
            verticalAlign = "top",
            align = "center",
            valueDecimals = 0
  )

hcc <- ds |> 
  count(age_group, marital_status) |> 
  hchart('column', hcaes(x = 'age_group', y = 'n', group = 'marital_status'),
  stacking = "normal") |> 
  hc_title(text = "Marital Status By Age Group") 
hcc |> hc_add_theme(hc_theme_elementary())
hcc |> hc_add_theme(hc_theme_monokai())

# pie chart gender  ----------
gender_data <- ds |> 
  count(gender) 

gender_data |> 
  hchart(
    "pie", hcaes(x = gender, y = n),
    name = "Gender",
    showInLegend = TRUE) |> 
  hc_colors(c("#a37ac2", "#0073C2FF")) |> 
  hc_title(text = 'Marital Status By Age Group',
           style = list(color = "#2baba0", 
                        fontFamily = "roboto", 
                        fontSize = "25px")) |> 
  hc_legend(layout = "horizontal",
            verticalAlign = "top",
            align = "center",
            valueDecimals = 0,
            itemStyle = list(
              fontSize = "20px",  
              fontWeight = "bold"))

# donut chart gender  ----------

gender_data |> 
  hchart(
    "pie", hcaes(x = gender, y = n),
    name = "Gender",
    showInLegend = TRUE) |>
  hc_plotOptions(
    pie = list(
      innerSize = "60%" )) |> 
  hc_colors(c("#a37ac2", "#0073C2FF")) |> 
  hc_title(text = 'Marital Status By Age Group',
           style = list(color = "#2baba0", 
                        fontFamily = "roboto", 
                        fontSize = "25px")) |> 
  hc_legend(layout = "horizontal",
            verticalAlign = "top",
            align = "center",
            valueDecimals = 0,
            itemStyle = list(
              fontSize = "20px",  
              fontWeight = "bold")
            )
  
  
# Histogram / Density of sales amounts  -------------

dsa <- ds |> filter(!is.na(amount))
dsa_male <- ds |> filter(!is.na(amount), gender == 'M') |> pull(amount)
dsa_female <- ds |> filter(!is.na(amount), gender == 'F') |> pull(amount)
dsa |> 
  ggplot(aes(amount)) +
  geom_histogram()

hchart(
  density(dsa$amount), 
  type = "area", name = "Amount") |> 
  hc_legend(enabled = FALSE) |> 
  hc_title(text = "Distribution of Sales Amount") |> 
  hc_add_theme(hc_theme_google())

hchart(
  density(dsa_male), 
  type = "area", 
  color = "#0073C2FF", name = "Male") |> 
  hc_add_series(
    density(dsa_female), type = "area",
    color = "#a37ac2",  name = "Female") |> 
  hc_xAxis(title = "") |> 
  hc_title(text = 'Distribution of Sales Amounts',
           style = list(color = "darkblue", 
                        fontFamily = "ubuntu", 
                        fontSize = "30px")) |> 
  hc_tooltip(enabled = FALSE) |> 
  hc_legend(layout = "vertical",
            verticalAlign = "top",
            align = "right",
            valueDecimals = 0,
            itemStyle = list(
              fontSize = "18px",  
              fontWeight = "bold")
  )


hchart(
  dsa$amount, 
  color = "royalblue", name = "Weight") |> 
  hc_xAxis(title = "") |> 
  hc_title(text = 'Distribution of Sales Amounts',
           style = list(color = "darkblue", 
                        fontFamily = "ubuntu", 
                        fontSize = "30px")) |> 
  hc_legend(enabled = FALSE)


# scatter plot: age vs amount -------------
mt <- mtcars |> tibble() |> 
  mutate(cyl = factor(cyl),
         am = recode_factor(am, `1` = 'Manual', `0` = 'Auto'))
mt
highchart() |>
  hc_add_series(
    data = mt,
    type = "scatter",
    hcaes(x = mpg,  y = disp, 
          color = cyl,  size = carb))
highchart() |>
  hc_add_series(
    data = mt,
    type = "scatter",
    hcaes(x = mpg,  y = disp, 
          color = cyl,  group = am)) 


# Boxplot -  violon plot  --------------

dsa |> 
  ggplot(aes(zone, amount)) +
  geom_boxplot()

dsa |> 
  ggplot(aes(gender, amount)) +
  geom_violin()


dsa
hcboxplot(x = dsa$amount, var = dsa$zone, color = 'dodgerblue') # deprecated !!!
dsa |> 
  data_to_boxplot(
    variable = amount, 
    group_var = zone, 
    add_outliers = FALSE,
    name = "Sales Amount")
  
bx_amount <- data_to_boxplot(dsa, amount, group_var = zone, add_outliers = FALSE)
highchart() |>
  hc_xAxis(type = "category") |>
  hc_add_series_list(bx_amount) |> 
  hc_legend(enabled = FALSE)

bx_amount_gdr <- 
  dsa |> 
  data_to_boxplot(
    variable = amount,   
    group_var = age_group, 
    group_var2 = gender,
    add_outliers = TRUE,
    name = "Sales Amount")
highchart() |>
  hc_xAxis(type = "category") |> 
  hc_add_series_list(bx_amount_gdr) |> 
  hc_colors(c(mcols[1], mcols[4])) |> 
  hc_title(text = "Total Amount By Age Group") |>
  hc_subtitle(text = "Camprison By Gender") |>
  hc_add_theme(hc_theme_bloom()) 




# Pyramid chart ------------------
pyramid_data <- ds |> 
  group_by(age_group, gender) |> 
  summarise(amount = sum(amount, na.rm = T), .groups = 'drop') |> 
  pivot_wider(names_from = gender, values_from = amount) |> 
  mutate(M = M *  (-1))

highchart() |>
  hc_chart(type = "bar") |>
  hc_title(text = "Total Amount by Age group adn Gender") |>
  hc_xAxis(categories = pyramid_data$age_group) |> 
  hc_add_series(
    name = "Positive",
    data = pyramid_data$F,
    color = "blue",
    showInLegend = FALSE
  ) |>
  hc_add_series(
    name = "Negative",
    data = pyramid_data$M,
    color = "red",
    showInLegend = FALSE
  )

pd = pyramid_data |>
  pivot_longer(-age_group, names_to = 'gender', values_to = 'amount') |> 
  mutate(gender = fct_relevel(factor(gender), 'M', 'F'))  # to align bar position to legend
pd |> 
  hchart('bar', 
         hcaes(x = 'age_group', y = 'amount', group = 'gender'),
         stacking = "normal") |> 
  hc_colors(c(mcols[1], mcols[4])) |> 
  hc_title(text = "Total Amount by Age group and Gender") |>
  hc_xAxis(title = "") |> 
  hc_yAxis(title = "")




# Polar plot: --------------
dsa
by_zone <- dsa |> count(zone, name = 'total') |> arrange(desc(total))
by_zone_rev <- dsa |> count(zone, name = 'total') |> arrange(total)
zone_gdr_avg <- dsa |> 
  group_by(zone, gender) |> 
  summarise(avg = mean(amount), .groups = 'drop') |> 
  pivot_wider(names_from = gender, values_from = avg) |> 
  rename(female = 2, male = 3)

by_zone |> hchart('bar', hcaes(x = zone, y = total))

highchart() |> 
  hc_chart(type = "bar",
           inverted = TRUE,
           polar = TRUE) |> 
  hc_add_series( data = by_zone$total) |> 
  hc_yAxis(tickLength = 0, gridLineColor = 'transparent') |> 
  hc_xAxis( 
    categories = by_zone$zone,
    tickLength = 0,
    gridLineColor = 'transparent') |> 
  hc_title(text = "Frequency Of Buyers By Zone") |>
  hc_add_theme(hc_theme_darkunica())

highchart() |> 
  hc_chart(type = "bar",
           inverted = TRUE,
           polar = TRUE) |> 
  hc_series(
    list(
      data = by_zone$total,
      colorByPoint = TRUE)) |> 
  hc_yAxis(tickLength = 0, gridLineColor = 'transparent') |> 
  hc_xAxis( 
    categories = by_zone$zone,
    tickLength = 0,
    gridLineColor = 'transparent') |> 
  hc_title(text = "Frequency Of Buyers By Zone") |>
  hc_add_theme(hc_theme_economist())


highchart() |> 
  hc_chart(type = "column",
           inverted = TRUE,
           polar = TRUE) |> 
  hc_plotOptions(column = list(stacking = "normal")) |> 
  hc_add_series( data = by_zone_rev$total) |> 
  hc_yAxis(tickLength = 0, gridLineColor = 'transparent') |> 
  hc_xAxis(
    categories = by_zone_rev$zone,
    tickLength = 0, gridLineColor = 'transparent') |> 
  hc_title(text = "Frequency Of Buyers By Zone") |>
  hc_add_theme(hc_theme_smpl())

highchart() |> 
  hc_chart(type = "column",
           inverted = TRUE,
           polar = TRUE) |> 
  hc_plotOptions(column = list(stacking = "normal")) |> 
  hc_add_series(data = zone_gdr_avg$female, name = 'Female') |> 
  hc_add_series( data = zone_gdr_avg$male, name = 'Male') |> 
  hc_yAxis(tickLength = 0, gridLineColor = 'transparent') |> 
  hc_xAxis(
    categories = zone_gdr_avg$zone,
    tickLength = 0, gridLineColor = 'transparent') |> 
  hc_title(text = "Average Amount By Zone") |>
  hc_subtitle(text = "Comparing Male and Female Buyers") |>
  hc_add_theme(hc_theme_ffx())

# Radar plot --------
highchart() |> 
  hc_chart(polar = TRUE) |> 
  hc_xAxis(categories = by_zone_rev$zone,
           tickmarkPlacement = "on",
           lineWidth = 0) |> 
  hc_yAxis(gridLineInterpolation = "polygon",
           lineWidth = 0,
           min = 0) |> 
  hc_series(
    list(
      name = "Amount",
      data = by_zone_rev$total,
      pointPlacement = "on", # rotation
      colorByPoint = TRUE,
      type = "column")) |> 
  hc_title(text = "Frequency Of Buyers By Zone") |>
  hc_add_theme(hc_theme_superheroes())


# Save gif ----------------------------------------------------------------

plot_path <- paste0(
  "/Users/birusod/Documents/DataScienceDocs/GitProjects/R4DS/rds_2023",
  "/2023/w46_Diwali_Sales"
)

fs::dir_copy(plot_path, "plots_w46")

# gg_playback(
#   name = file.path("2023", "2023-11-14", paste0("20231114", ".gif")),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .25,
#   background = bg_col
# )
