# creating a template for tidytuesday script
# inspired by Nicola Rennie's blog post 
# https://nrennie.rbind.io/blog/script-templates-r/


# only need to update these parameters and Alt+Cmd+r
# ****************************************************************
dd <- '2023-10-10'
wk <- 'w41_US_Haunted_Places'
tt <- 'Haunted Places in the United States'
#*****************************************************************

use_tt_template <- function(
    date_chr = dd, week = wk, title = tt, readme = TRUE) {
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
readme_txt <- gsub(pattern = "date_chr", replacement = date_chr, x = readme_txt)
readme_txt <- gsub(pattern = "date_strip", replacement = date_strip, x = readme_txt)

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





