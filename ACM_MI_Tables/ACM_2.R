
# one_source_pro <- one_source_pro %>%
#   mutate(Tenure = factor(Tenure),
#          CombinedCategory = factor(CombinedCategory, levels = c('Remediation completed', 'Works complete awaiting building control signoff', 'Remediation started - cladding removed',
#                                                                               'Remediation started', 'Plans in place', 'Responded with intent', 'Remediation plan unclear') ))
# #these are the row names used in the MI tables
# levels(one_source_pro$CombinedCategory) <- c('Completed Remediation', 'Works complete awaiting building control signoff', 'Remediation started - cladding removed', 'Remediation started', 'Remediation plans in place'
#                              , 'Reported an intent to remediate', 'Remediation plan unclear')


ACM_2 <- list(one_source_pro %>% filter(Tenure == 'social') %>% count(CombinedCategory, name  = 'Social sector residential Number'),
                         
                         one_source_pro %>% filter(Tenure == 'private residential') %>% count(CombinedCategory, name  = 'Private sector residential Number'),
              
                         one_source_pro %>% filter(Tenure == 'student') %>% count(CombinedCategory, name  = 'Student accommodation Number'),
              
                         one_source_pro %>% filter(Tenure == 'hotel') %>% count(CombinedCategory, name  = 'Hotels Number'),
              
                         one_source_pro %>% filter(Tenure == 'public') %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
              
                         one_source_pro %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Remediation Category` = `CombinedCategory` ) %>%
  # 
  # mutate(`Remediation Category` = factor(`Remediation Category`, levels = c('Completed Remediation', 'Works complete awaiting building control signoff', 'Remediation started - cladding removed', 'Remediation started', 'Remediation plans in place'
  #                                                                           , 'Reported an intent to remediate', 'Remediation plan unclear', 'Total buildings'))) %>%
  #replace NA with 0
  replace(is.na(.), 0) %>%
  
  #calculate the percentage of the total
  mutate(
    `Social sector residential Percentage (%)` = (`Social sector residential Number` / sum(`Social sector residential Number`)),
    `Private sector residential Percentage (%)` = (`Private sector residential Number` / sum(`Private sector residential Number`)),
    `Student accommodation Percentage (%)` = (`Student accommodation Number` / sum(`Student accommodation Number`)),
    `Hotels Percentage (%)` = (`Hotels Number` / sum(`Hotels Number`)),
    `Publicly-owned buildings Percentage (%)` = (`Publicly-owned buildings Number` / sum(`Publicly-owned buildings Number`)),
    `Total: all tenures Percentage (%)` = (`Total: all tenures Number` / sum(`Total: all tenures Number`))) %>%
  
  #reorder the columns
  select(
    `Remediation Category`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Remediation Category`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Remediation Category` = "Total buildings",
              `Social sector residential Number` = sum(`Social sector residential Number`),
              `Social sector residential Percentage (%)` = 1,
              `Private sector residential Number` = sum(`Private sector residential Number`),
              `Private sector residential Percentage (%)` = 1,
              `Student accommodation Number` = sum(`Student accommodation Number`),
              `Student accommodation Percentage (%)` = 1,
              `Hotels Number` = sum(`Hotels Number`),
              `Hotels Percentage (%)` = 1,
              `Publicly-owned buildings Number` = sum(`Publicly-owned buildings Number`),
              `Publicly-owned buildings Percentage (%)` = 1,
              `Total: all tenures Number` = sum(`Total: all tenures Number`),
              `Total: all tenures Percentage (%)` = 1
    )
  )

