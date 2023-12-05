library(tidyverse)


# Load fonts --------------------------------------------------------------

font_add_google("Roboto", "roboto")
font_add_google("Fira code", "firac")
font_add_google("Fira sans", "firas")
font_add_google("Fira mono", "firam")
font_add_google("Archivo Black", "archivo")
font_add_google("Eater", "eater")
font_add_google("Ubuntu", "ubuntu")
font_add_google("Lobster Two", "lobstertwo")
font_add_google("Caladea", "caladea")
font_add_google("Outfit", "outfit")
font_add_google(name = "IM Fell DW Pica", family = "IM Fell DW Pica")
pica <- "IM Fell DW Pica"
font_add_google(name = "Lato", family = "Lato")
lato <- "Lato"

showtext_auto()


# Define colors --------------------------------------------------

bg_col <- ""
text_col <- ""
highlight_col <- ""

tcols <- c("#476BA0", "#AA0000", "#B2ADA3",  "#68AA63",  
           "#A58C30", "#C9C977", "#DBD83D",  "#BAD8EA")
mcols <- c("#0073C2FF", "#EFC000FF", "#603C14", "#CD4F41", "#2D8077",
           "#BC8F8F", "#8FBC8F")
rcols <- c("#bb3e03","#ee9b00","#e9d8a6","#94d2bd","#0a9396","#005f73")

pcols <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
           '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
# pcols: muted blue-safety orange-cooked asparagus green-brick red
#        muted purple-chestnut brown-raspberry yogurt pink
#        middle gray-curry yellow-green-blue-teal


library(colorspace)
# https://datacadamia.com/lang/r/colorspace
# https://cran.r-project.org/web/packages/colorspace/vignettes/colorspace.html#Usage_with_ggplot2

cscols <- choose_palette()
hcl_palettes(plot = TRUE)
qualitative_hcl(4, palette = "Dark 3")
sequential_hcl(4, palette = 'Dark Mint')
diverge_hcl(4, palette = 'Tropic')


# https://emilhvitfeldt.com/post/paletteer-version-1-0-0/
# https://emilhvitfeldt.github.io/paletteer/

library(paletteer)
paletteer_d("nord::frost")
paletteer_d("MetBrewer::Tsimshian")
paletteer_d("MetBrewer::Monet")
paletteer_d("wesanderson::Rushmore1")

library(PrettyCols)
# https://nrennie.github.io/PrettyCols/reference/index.html
view_all_palettes()
mtcars |> 
  ggplot(aes(cyl, fill  = factor(cyl))) +
  geom_bar()  +
  scale_fill_pretty_d('Dark')

# https://cararthompson.github.io/ophelia/index.html
# token required for the Ophelia install
palmerpenguins::penguins |>
  ggplot(aes(x = 1,
             fill = species),
         stat = "count") +
  geom_bar() +
  xlim(c(-0.5, 2)) +
  coord_polar(theta = "y") +
  labs(fill = '',
       title = "Perfectly proportional penguins",
       subtitle = "Does anyone know if penguins like donuts?",
       caption = "Data from {palmerpenguins}") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_pretty_d('Dark') +
  theme(plot.title = ggtext::element_markdown(hjust = 0.5),
        plot.subtitle = ggtext::element_textbox_simple(halign = 0.5, vjust = 1),
        legend.position = "right")

sessionInfo()
