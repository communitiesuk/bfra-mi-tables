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

# filepath for last month's master analytic
month_name <- month.name[as.numeric(month)]
acm_raw_path = file.path("D:", "Users", Sys.getenv("USERNAME"),"OneDrive - MHCLG","BSP Data and Analysis - Publication of Information")
is_dir(acm_raw_path)
acm_raw_final_path = file.path(acm_raw_path, year, paste(month_name, year), 'ACM')
is_dir(acm_raw_final_path)

#list.files uses the regex pattern to select the matching file
master_analytical = file.path(acm_raw_final_path, list.files(acm_raw_final_path, '\\d{8} ACM MASTER ANALYTICAL.xlsx'))

# read in this month's OSP
one_source_pro <- read_excel(master_analytical, sheet = 'One_source_pro')

# filepath for last month's master analytic
last_month_name <- month.name[as.numeric(last_month)]

last_year <- if (last_month == '12') {
  last_year <- as.character(as.numeric(year) - 1)
} else
{
  last_year  = year
}

acm_raw_final_path = file.path(acm_raw_path, year, paste(last_month_name, last_year), 'ACM')
master_analytical_last_month = file.path(acm_raw_final_path, list.files(acm_raw_final_path, '\\d{8} ACM MASTER ANALYTICAL.xlsx'))

# read in last month's OSP
one_source_pro_last_month <-read_excel(master_analytical_last_month, sheet = 'One_source_pro')

#sets variables as factors so count() works correctly
source(file.path("D:", "Users", Sys.getenv("USERNAME"), 'Github','sgba-mi-tables', 'ACM_MI_Tables', 'create_OSP_factors.R'))

#create the ACM MI tables
source(file.path("D:", "Users", Sys.getenv("USERNAME"), 'Github','sgba-mi-tables', 'ACM_MI_Tables', 'ACM_1.R'))
source(file.path("D:", "Users", Sys.getenv("USERNAME"), 'Github','sgba-mi-tables', 'ACM_MI_Tables', 'ACM_2.R'))
source(file.path("D:", "Users", Sys.getenv("USERNAME"), 'Github','sgba-mi-tables', 'ACM_MI_Tables', 'ACM_3.R'))
source(file.path("D:", "Users", Sys.getenv("USERNAME"), 'Github','sgba-mi-tables', 'ACM_MI_Tables', 'ACM_4.R'))
source(file.path("D:", "Users", Sys.getenv("USERNAME"), 'Github','sgba-mi-tables', 'ACM_MI_Tables', 'ACM_5.R'))
source(file.path("D:", "Users", Sys.getenv("USERNAME"), 'Github','sgba-mi-tables', 'ACM_MI_Tables', 'ACM_6.R'))
source(file.path("D:", "Users", Sys.getenv("USERNAME"), 'Github','sgba-mi-tables', 'ACM_MI_Tables', 'ACM_8.R'))
source(file.path("D:", "Users", Sys.getenv("USERNAME"), 'Github','sgba-mi-tables', 'ACM_MI_Tables', 'ACM_9.R'))
source(file.path("D:", "Users", Sys.getenv("USERNAME"), 'Github','sgba-mi-tables', 'ACM_MI_Tables', 'ACM_11.R')) #will overwrite the ACM_11.xlsx file 