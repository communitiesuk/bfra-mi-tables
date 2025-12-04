#ACM_11 is a time series showing remediation category by month  
old_number_data <- read_excel('Q:\\BSP\\Automation\\MI Tables\\ACM_11.xlsx', sheet = 'Number')
old_percentage_data <- read_excel('Q:\\BSP\\Automation\\MI Tables\\ACM_11.xlsx', sheet = 'Percentage')

#Create the abbreviated name for the column headers, e.g. Jan-26 Number
month_short <- substr(month_name, 1, 3)
year_short <- substr(year, 3, 4)
month_year_short_no <- paste(month_short, '-', year_short, '\n Number', sep = '')
month_year_short_pct <- paste(month_short, '-', year_short, '\n Percentage (%)', sep = '')

#calculate the latest month's data on number of buildings remediated by category
new_number_data <-
  one_source_pro %>%
  count(CombinedCategory, name = month_year_short_no) %>%
  rename(`Remediation Category [note 63]` = `CombinedCategory`) %>%
  bind_rows(
    summarise(.,
              `Remediation Category [note 63]` = "Total buildings",
              #!! means that R reads the strong contained within month_year_short_no instead of interpreting the name literally. := means that the column name can be read from a variable.
              #.data[[month_year_short_no]] means "select the column whose name is stored in the variable month_year_short_no." .data refers to the data currently being operated on.
              !!month_year_short_no := sum(.data[[month_year_short_no]], na.rm = TRUE)
    )
  ) %>%
  select(!!month_year_short_no)

#calculate the latest month's data on percentage of buildings in each category
new_percentage_data <- one_source_pro %>%
  count(CombinedCategory, name = month_year_short_pct) %>%
  rename(`Remediation Category [note 10]` = `CombinedCategory`) %>%
  #calculate the percentages of the total in each row
  mutate(
    !!month_year_short_pct := .data[[month_year_short_pct]] / sum(.data[[month_year_short_pct]])) %>%
  bind_rows(
    summarise(.,
              `Remediation Category [note 10]` = "Total buildings",
              !!month_year_short_pct := sum(.data[[month_year_short_pct]], na.rm = TRUE)
    )
  ) %>%
  select(!!month_year_short_pct)

#ACM_11_1 is the number time series - add on the latest month's data
ACM_11_1 <- add_column(old_number_data, new_number_data)

#ACM_11_2 is the percentage time series - add on the latest month's data
ACM_11_2 <- add_column(old_percentage_data, new_percentage_data)            

ACM_11 <- list(ACM_11_1, ACM_11_2)

#append the latest month's data to the historical time series
old_number_data[month_year_short_no] <- new_number_data
old_percentage_data[month_year_short_pct] <- new_percentage_data

#recreate the ACM_11.xlsx sheet with the latest month's data and then save it
wb <- createWorkbook()

addWorksheet(wb, 'Number')
writeData(wb, 'Number', old_number_data)

addWorksheet(wb, 'Percentage')
writeData(wb, 'Percentage', old_percentage_data)


saveWorkbook(wb, "Q:\\BSP\\Automation\\MI Tables\\ACM_11.xlsx", overwrite = TRUE)