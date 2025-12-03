#these LAs are within greater Manchester
manchester_LAs <- c('Tameside', 'Bolton', 'Bury', 'Manchester', 'Oldham', 'Rochdale', 'Salford', 'Stockport', 'Trafford', 'Wigan')

#the remaining table contains buildings in the rest of the country
one_source_pro_rest <- one_source_pro %>% filter(! LocalAuthority %in% manchester_LAs & ! str_detect(LocalAuthorityCode, '^E09'))

#ACM_5_1 tracks buildings in Manchester by remediation stage and tenure
ACM_5_1 <- list(one_source_pro %>% filter(Tenure == 'social' & LocalAuthority %in% manchester_LAs) %>% count(CombinedCategory, name  = 'Social sector residential Number', .drop = FALSE),
                
                one_source_pro %>% filter(Tenure == 'private residential' & LocalAuthority %in% manchester_LAs) %>% count(CombinedCategory, name  = 'Private sector residential Number'),
                
                one_source_pro %>% filter(Tenure == 'student' & LocalAuthority %in% manchester_LAs) %>% count(CombinedCategory, name  = 'Student accommodation Number'),
                
                one_source_pro %>% filter(Tenure == 'hotels' & LocalAuthority %in% manchester_LAs) %>% count(CombinedCategory, name  = 'Hotels Number'),
                
                one_source_pro %>% filter(Tenure == 'public' & LocalAuthority %in% manchester_LAs) %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
                
                one_source_pro %>% filter(LocalAuthority %in% manchester_LAs) %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Manchester` = `CombinedCategory` ) %>%

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
    `Manchester`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  

  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Manchester`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Manchester` = "Total buildings",
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



#ACM_5_2 tracks buildings in London by remediation stage and tenure - London LA codes start with E09
ACM_5_2 <- list(one_source_pro %>% filter(Tenure == 'social' & str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name  = 'Social sector residential Number', .drop = FALSE),
                
                one_source_pro %>% filter(Tenure == 'private residential' & str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name  = 'Private sector residential Number'),
                
                one_source_pro %>% filter(Tenure == 'student' & str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name  = 'Student accommodation Number'),
                
                one_source_pro %>% filter(Tenure == 'hotel' & str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name  = 'Hotels Number'),
                
                one_source_pro %>% filter(Tenure == 'public' & str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
                
                one_source_pro %>% filter(str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`London` = `CombinedCategory` ) %>%
  
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
    `London`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%

  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`London`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `London` = "Total buildings",
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


#ACM_5_3 tracks buildings in the rest of England by remediation stage and tenure
ACM_5_3 <- list(one_source_pro %>% filter(Tenure == 'social' & ! LocalAuthority %in% manchester_LAs & ! str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name  = 'Social sector residential Number'),
                
                one_source_pro %>% filter(Tenure == 'private residential' & ! LocalAuthority %in% manchester_LAs & ! str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name  = 'Private sector residential Number', .drop = FALSE),
                
                one_source_pro %>% filter(Tenure == 'student' & ! LocalAuthority %in% manchester_LAs & ! str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name  = 'Student accommodation Number'),
                
                one_source_pro %>% filter(Tenure == 'hotel' & ! LocalAuthority %in% manchester_LAs & ! str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name  = 'Hotels Number'),
                
                one_source_pro %>% filter(Tenure == 'public' & ! LocalAuthority %in% manchester_LAs & ! str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
                
                one_source_pro %>% filter(! LocalAuthority %in% manchester_LAs & ! str_detect(LocalAuthorityCode, '^E09')) %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Rest of England` = `CombinedCategory` ) %>%
  
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
    `Rest of England`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Rest of England`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Rest of England` = "Total buildings",
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
ACM_5 <- list(ACM_5_1, ACM_5_2, ACM_5_3)
