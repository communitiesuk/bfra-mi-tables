rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)
library(tibble)
library(openxlsx)


is_dir <- function(path) {
  if(!dir.exists(path)){
    stop("This path is invalid.")
  }
}

##### Entry Point #####
year = "2025"
month = "11" # Change this depending on the monthly run (01 - Jan, 12 - Dec)
last_month = "10" # change this to last month

# filepath for this month's CSS export
month_name <- month.name[as.numeric(month)]
css_raw_path = file.path("D:", "Users", Sys.getenv("USERNAME"),"OneDrive - MHCLG","BSP Data and Analysis - Publication of Information", year, paste(month_name, year))
is_dir(css_raw_path)

#list.files uses the regex pattern to select the matching file
sos_export = file.path(css_raw_path, list.files(css_raw_path, paste0('CSS data end ', month_name, '.xlsx')))

# read in this month's CSS data
css_data <- read_excel(sos_export, sheet = 'SOS')
