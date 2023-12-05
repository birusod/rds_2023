# creating a template for tidytuesday script
# inspired by Nicola Rennie's blog post 
# https://nrennie.rbind.io/blog/script-templates-r/


# only need to update these 5 parameters, then: Alt+Cmd+r (Cmd+A+Enter)
# ****************************************************************
week_date <- '2023-11-28'
current_week_folder <- 'w48_Doctor_Who'
week_title <- 'Doctor Who Episodes'
plot_folder <- 'plots_w48'
final_plot <- 'final_plot_w48.png'
#*****************************************************************

use_tt_template <- function(
    date_chr = week_date, 
    week = current_week_folder, 
    title = week_title, 
    readme = TRUE) {
  # check date in correct format
  if (is.na(as.Date(date_chr, format = "%Y-%m-%d"))) {
    stop("'date_chr' in incorrect format. Should be yyyy-mm-dd.")
  }

# Creating folders and files
#title <- title #'Fair Use'
#week <-  week #'w35_Fair Use'
#date_chr <- date # "2023-08-29"
yr <- sub("-.*", "", date_chr)
date_strip <- stringr::str_remove_all(date_chr, "-")



new_folder <- file.path(yr, week)

# check if folder exist (to avoid overwriting)
if (file.exists(new_folder)) {
  stop("Please check that date and folder name are correct!")
}

if (!file.exists(new_folder)) {
  dir.create(new_folder, recursive = TRUE)
  message("Created new folder")
}



# create r file
new_file <- file.path(new_folder, paste0(date_strip, ".R"))

if (!file.exists(new_file)) {
  file.create(new_file)
  message("Created '.R' file")
}

# create a  new readme.md file
new_readme <- file.path(new_folder, "README.md")
if (!file.exists(new_readme)) {
  file.create(new_readme)
  message("Created 'README.md' file")
}


# Creating a template README file: readme-template.md 


# copy lines to README file
readme_txt <- readLines("readme-template.md")

# replace placeholder text with variables
readme_txt <- gsub(pattern = "title", replacement = title, x = readme_txt)
readme_txt <- gsub(pattern = "yr", replacement = yr, x = readme_txt)
readme_txt <- gsub(pattern = "wkf", replacement = current_week_folder, x = readme_txt)
readme_txt <- gsub(pattern = "pfolder", replacement = plot_folder, x = readme_txt)

# write to file
writeLines(readme_txt, con = new_readme)
message("'README.md' contents copied")


# Creating a template .R file r-template.R

# Write to new r file: yyyymmdd.R
# copy lines to .R file
r_txt <- readLines("r-template.R")

# replace placeholder text with variables
r_txt <- gsub(pattern = "yr", replacement = paste0("\"", yr, "\""), x = r_txt)
r_txt <- gsub(pattern = "date_chr", replacement = paste0("\"", date_chr, "\""), x = r_txt)
r_txt <- gsub(
  pattern = "date_strip",
  replacement = paste0("\"", date_strip, "\""), 
  x = r_txt)

# write to new file
writeLines(r_txt, con = new_file)
message("'.R' contents copied")

}


# Excute function for new folder, file, readme:
use_tt_template()





