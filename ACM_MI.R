rm(list = ls())

library(dplyr)
library(tidyr)
library(readxl)

is_dir <- function(path) {
  if(!dir.exists(path)){
    stop("This path is invalid.")
  }
}

##### Entry Point #####
year = "2025"
month = "10" # Change this depending on the monthly run (01 - Jan, 12 - Dec)
last_month = "09" # change this to last month

# copy in this months data
month_name <- month.name[as.numeric(month)]

acm_raw_path = file.path("D:", "Users", Sys.getenv("USERNAME"),"OneDrive - MHCLG","BSP Data and Analysis - Publication of Information")
is_dir(acm_raw_path)
acm_raw_final_path = file.path(acm_raw_path, year, paste(month_name, year), 'ACM')
is_dir(acm_raw_final_path)

master_analytical = list.files(acm_raw_final_path, '\\d{8} ACM MASTER ANALYTICAL.xlsx')

# Read Excel file - this month
one_source_pro <- read_excel(master_analytical, sheet = 'One_source_pro')