pacman::p_load(sp, sf, maptools, mapproj, maps, rnaturalearth,
               ggiraph, 
               tidyverse, ggthemes, janitor,
               WDI)

# Interactive Africa map with ggigraph -------

# rnaturalearth
afr_map_data <- 
  sf::st_as_sf(countries110) |> 
  clean_names() |> 
  filter(region_un == 'Africa') |> 
  st_transform(crs = "+proj=laea +lon_0=18.984375")

# sp [large => more detailed]
sp::plot(ne_countries(type = "countries", scale = "small"))
sp::plot(ne_countries(continent = "africa", scale = "medium")) 
sp::plot(ne_countries(continent = "africa", scale = "small"))

# sf
plot(afr_map_data)  # plotting the first 10 out of 168 attributes
plot(afr_map_data["tlc"])
plot(afr_map_data["admin"])

# WDI package: GDP per capita African 2021 to 2011 [NY.GDP.PCAP.CD]

gdp_df <- WDI(
  indicator = c("NY.GDP.PCAP.CD"),
  start = 2010,
  end = 2021,
  extra = T
  ) |> 
  filter(
    year  %in% c(2011,2021),
    iso3c %in% afr_map_data$iso_a3) |> 
  select(iso3c, year, value = NY.GDP.PCAP.CD) |> 
  pivot_wider(
    id_cols = c('iso3c'),
    names_from = 'year',
    values_from = 'value',
    names_prefix = 'y') |> 
  mutate(chg_rt =  (y2021 - y2011) / y2011)


# merge data for the map:   
afr_dt <- afr_map_data |> left_join(gdp_df,by = c("iso_a3"="iso3c"))

afr.centers <- st_centroid(afr_dt)
afr.spdf <- methods::as(afr_dt, 'Spatial')
afr.spdf@data$id <- row.names(afr.spdf@data)


afr.tidy <- afr.spdf |> broom::tidy()  #sf::st_as_sf(afr.spdf)
afr.tidy2 <- dplyr::left_join(afr.tidy, afr.spdf@data, by = 'id')

afr.tidy3 <- afr.tidy2  |>  
  mutate(labs = ifelse(
    is.na(chg_rt) == T, 
    NA, 
    paste0(round(100 * chg_rt, 1),"%"))) |> 
  mutate(chg_rt = 100 * chg_rt,
         tip = ifelse(
           is.na(chg_rt) == T,
           paste0("<b>",name,"</b><br>","Data not available"),
           paste0("<b>",name,"</b><br>",labs)))


# map ggplot
afr.tidy3 |> head() |> broom::tidy()
afr.tidy3 |> select(starts_with('group'))
g <- 
  afr.tidy3 |> 
  ggplot() +
  geom_polygon_interactive(
    aes(
      long, lat,
      group = group, 
      fill = chg_rt, tooltip = tip
      ),
    color = 'grey70',size = .1) +
  guides(fill = guide_legend(
    title = "10 Years \n GDP per capita \n growth rate")) +
  theme_bw() +
  coord_fixed() +
  scale_fill_gradientn(colours = c("red","white","blue"), 
                       na.value = 'lightgray',
                       values = scales::rescale(c(-80,0,180))) +
  labs(title = 'GDP per capita growth rate \n from 2011 to 2021') +
  theme_tufte(ticks = FALSE) + 
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

# ggiraph
# unction `ggiraph()` is replaced by `girafe()` and will be removed soon.
ggiraph(
  code = print(g),
  width = .8,
  width_svg = 5,
  height_svg = 5,
  tooltip_opacity = 0.7,
  hover_css = "{fill:orange;}")

girafe(
  code = print(g),
  width = .8,
  width_svg = 5,
  height_svg = 5)

data("mtcars")
mgdt <- mtcars |> 
  mutate(cyl = factor(cyl)) |> 
  group_by(cyl) |> 
  summarise(mpg = mean(mpg)) |> 
  mutate(cyl = fct_reorder(cyl, mpg)) 

mg <- mgdt|> 
  ggplot(aes(cyl, mpg, 
             tooltip = paste0(cyl, '\n', round(mpg, 1)),
             data_id = cyl)) +
  geom_col_interactive(fill = 'dodgerblue') +
  #geom_col() +
  scale_fill_brewer(palette = 'Set1') +
  theme_minimal() +
  theme(legend.position = 'none')
mg
girafe(code = print(mg),
       width = .4,
       width_svg = 5,
       height_svg = 4)
