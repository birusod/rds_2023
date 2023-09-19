# Load packages -----------------------------------------------------------

pacman::p_load(
  tidytuesdayR, tidyverse, janitor, scales, 
  d3r, sunburstR, highcharter, treemap,
  showtext, patchwork, ggtext, glue, cowplot, patchwork,
  ggthemes, camcorder)

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2023-09-12")
tuesdata

all_countries <- tuesdata$all_countries |> head() |> view()

dat <- all_countries |> 
  select(level1 = Category,
         level2 = Subcategory,
         size  = hoursPerDayCombined) |> 
  group_by(level1, level2) |> 
  summarise(size = mean(size), .groups = 'drop')


# Sunburst: d3r, sunburstR ------------------------------------------------

tree <- d3_nest(dat, value_cols = "size")

sb1 <- sunburst(
  tree, 
  width = "100%", 
  height = 400,
  legend = list(w = 200, h = 20, r = 20, s = 5) # width,height,spacing,radius (px)
  )
sb1

# Sunburst: d3r, sunburstR ------------------------------------------------
sb2 <- sund2b(tree, 
              width = "100%",
              showLabels = TRUE)
sb2


# highcharter

dout <- 
  dat |> 
  mutate(size = round(size, 2)) |> 
  data_to_hierarchical(c(level1, level2), size)

hchart(dout, type = "sunburst") |> 
  hc_title(
    text = "Global Human Day") |> 
  hc_subtitle(
    text = "The number of hours per day engaged in each activity") |> 
  hc_caption(
    text = "This is a long text to give some 
    subtle details of the data which can be relevant to the reader. 
    This is usually a long text that's why I'm trying to put a 
    <i>loooooong</i> text.", 
    useHTML = TRUE) |> 
  hc_credits(
    text = "Chart created using R and highcharter",
    href = "http://jkunst.com/highcharter",
    enabled = TRUE)


hchart(dout, type = "treemap") |> 
  hc_legend(
    align = "left",
    verticalAlign = "top",
    layout = "vertical",
    x = 0, y = 100
  ) |>
  hc_tooltip(
    crosshairs = TRUE,
    backgroundColor = "#F0F0F0",
    shared = TRUE, 
    borderWidth = 5
  )


## map with treemap package

data(GNI2014, package = "treemap")

df_gni <- GNI2014 |> select(iso3, GNI)
cdf <- 
  all_countries |> 
  select(iso3 = country_iso3, Category, Subcategory, hoursPerDayCombined)

df

hcmap()
hcmap(
  "custom/world-robinson-lowres", # add some curvature
  data = cdf,
  name = "Number of hours per day",
  value = "hoursPerDayCombined",
  borderWidth = 0,
  nullColor = "#d3d3d3",
  joinBy = c("iso-a3", "iso3")
) |>
  hc_colorAxis(
    # "#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"
    minColor = "#7AD151",
    maxColor = "#440154",
    min = 0, max = 2
  ) |> 
  hc_title(
    text = "Global Human Day") |> 
  hc_subtitle(
    text = "The number of hours per day engaged in each activity")


# hc donut plot
bycat <- cdf |> 
  group_by(Category) |> 
  summarise(avg = round(mean(hoursPerDayCombined), 1))

highchart() |>  
  hc_add_series(
    type = "pie", 
    data = bycat, 
    hcaes(Category, avg), 
    size = "60%", 
    name = "Hours per day", 
    #center = c(50, 80),
    innerSize = "50%",
    # dataLabels = list(distance = -50,
    #                   formatter = JS("function () 
    #                                  {return this.y > 5 ? 
    #                                  this.point.name : null;}"))
    ) |> 
  hc_title(
    text = "<b>Global Human Day</b>",
    style = list(color = "#22A884", useHTML = TRUE)) |>
  hc_subtitle(
    text = "<i>The number of hours per day engaged in each activity</i>",
    style = list(color = "#22A2DD", useHTML = TRUE)) |> 
  hc_plotOptions(
    innersize = "50%", 
    startAngle = 90, 
    endAngle = 10,
    center = list('50%', '75%'),
    size = '110%')


bycat |> 
  mutate(lab  = paste0(Category, "\n", avg)) |> 
  ggplot(aes(Category, avg)) +
  geom_bar(stat = 'identity') +
  geom_label(aes(label = lab)) +
  coord_polar() +
  theme_void()

bycat |> 
  mutate(
    Category = fct_reorder(Category, avg),
    shorttext = c('DNR', 'EO', 'FP', 'MS', 'NFP', 'ORG', 'SM', 'TM'),
    lab  = paste0(shorttext, " ", avg)) |> 
  ggplot(aes(Category, avg, fill = Category)) +
  geom_bar(stat = 'identity') +
  geom_label(aes(label = lab, y = max(avg)), hjust = "inward") +
  coord_polar(theta = 'y') +
  theme_void() +
  theme(
    legend.position = 'none',
    )

library(ggrepel)
bycat |> 
  mutate(shorttext = c('DNR', 'EO', 'FP', 'MS', 'NFP', 'ORG', 'SM', 'TM'),
         frac = avg  / sum(avg),
         ymax = cumsum(frac),
         ymin = c(0, head(ymax, n  = -1)),
         pos = (ymax + ymin) / 2,
         lab = paste0(avg)) |> 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Category)) +
  geom_rect() +
  geom_text_repel(
    x = 4.6, 
    aes(y = pos, label = lab), 
    size = 4, color = 'white', fontface = 'bold', 
    force_pull  = 3) +
  scale_fill_brewer(palette = 'Set1') +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +
  labs(
    title = "Global Human Day",
    subtitle = "The number of hours per day engaged in each activity") +
  theme_void() +
  theme(
    legend.position = "top",
    legend.text = element_text(color = 'white'),
    plot.title = element_text(hjust = .5, size = 20, 
                              color = 'darkorange', face = 'bold'),
    plot.subtitle = element_text(hjust = .5, size = 14, color = 'orange'),
    plot.background = element_rect(fill = 'black'),
    panel.spacing = unit(0,"null"),
    plot.margin = unit(c(0, 2, 0, 2), "inches")
    ) +
  guides(fill = guide_legend(
    ncol = 2, keywidth = .5, keyheight = .5,
    title = '')
    )
  
