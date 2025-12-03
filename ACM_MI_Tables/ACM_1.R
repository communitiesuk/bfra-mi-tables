# one_source_pro <- one_source_pro %>%
#   mutate(Tenure = factor(Tenure))
# 
# 
# one_source_pro_last_month <- one_source_pro_last_month %>%
#   mutate(Tenure = factor(Tenure)) %>%
#   rename(`Tenure Last Month` = `Tenure`)

ACM_1 <- full_join(
  one_source_pro %>%
    count(Tenure, name = 'Number of buildings this month'),
  
  one_source_pro_last_month %>%
    count(`Tenure Last Month`, name = 'Number of buildings last month') %>%
    rename(Tenure = `Tenure Last Month`),
  by = 'Tenure'
) %>%
  mutate(`Monthly Change` = `Number of buildings this month`  - `Number of buildings last month`) %>%
  #create total row
  bind_rows(
    summarise(.,
              Overall = "Total buildings",
              `Number of buildings this month` = sum(`Number of buildings this month`, na.rm = TRUE),
              `Number of buildings last month` = sum(`Number of buildings last month`, na.rm = TRUE),
              `Monthly Change` = sum(`Monthly Change`, na.rm = TRUE)
    )
  )
