#ACM_6_1 counts all buildings identified by 31 Dec 2019
ACM_6_1 <- list(one_source_pro %>% filter(Tenure == 'social' & InDec19 == TRUE) %>% count(CombinedCategory, name  = 'Social sector residential Number', .drop = FALSE),
              
              one_source_pro %>% filter(Tenure == 'private residential'& InDec19 == TRUE) %>% count(CombinedCategory, name  = 'Private sector residential Number'),
              
              one_source_pro %>% filter(Tenure == 'student'& InDec19 == TRUE) %>% count(CombinedCategory, name  = 'Student accommodation Number'),
              
              one_source_pro %>% filter(Tenure == 'hotel' & InDec19 == TRUE) %>% count(CombinedCategory, name  = 'Hotels Number'),
              
              one_source_pro %>% filter(Tenure == 'public'& InDec19 == TRUE) %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
              
              one_source_pro %>% filter(InDec19 == TRUE) %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Identified by 31 December 2019` = `CombinedCategory` ) %>%
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
    `Identified by 31 December 2019`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Identified by 31 December 2019`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Identified by 31 December 2019` = "Total buildings",
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


#ACM_6_2 counts all buildings identified by 31 Dec 2020
ACM_6_2 <- list(one_source_pro %>% filter(Tenure == 'social' & InDec20 == TRUE) %>% count(CombinedCategory, name  = 'Social sector residential Number', .drop = FALSE),
                
                one_source_pro %>% filter(Tenure == 'private residential'& InDec20 == TRUE) %>% count(CombinedCategory, name  = 'Private sector residential Number'),
                
                one_source_pro %>% filter(Tenure == 'student'& InDec20 == TRUE) %>% count(CombinedCategory, name  = 'Student accommodation Number'),
                
                one_source_pro %>% filter(Tenure == 'hotel' & InDec20 == TRUE) %>% count(CombinedCategory, name  = 'Hotels Number'),
                
                one_source_pro %>% filter(Tenure == 'public'& InDec20 == TRUE) %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
                
                one_source_pro %>% filter(InDec20 == TRUE) %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Identified by 31 December 2020` = `CombinedCategory` ) %>%
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
    `Identified by 31 December 2020`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Identified by 31 December 2020`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Identified by 31 December 2020` = "Total buildings",
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


#ACM_6_2 counts all buildings identified by 31 Dec 2020
ACM_6_2 <- list(one_source_pro %>% filter(Tenure == 'social' & InDec20 == TRUE) %>% count(CombinedCategory, name  = 'Social sector residential Number', .drop = FALSE),
                
                one_source_pro %>% filter(Tenure == 'private residential'& InDec20 == TRUE) %>% count(CombinedCategory, name  = 'Private sector residential Number'),
                
                one_source_pro %>% filter(Tenure == 'student'& InDec20 == TRUE) %>% count(CombinedCategory, name  = 'Student accommodation Number'),
                
                one_source_pro %>% filter(Tenure == 'hotel' & InDec20 == TRUE) %>% count(CombinedCategory, name  = 'Hotels Number'),
                
                one_source_pro %>% filter(Tenure == 'public'& InDec20 == TRUE) %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
                
                one_source_pro %>% filter(InDec20 == TRUE) %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Identified by 31 December 2020` = `CombinedCategory` ) %>%
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
    `Identified by 31 December 2020`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Identified by 31 December 2020`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Identified by 31 December 2020` = "Total buildings",
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


#ACM_6_3 counts all buildings identified by 31 Dec 2021
ACM_6_3 <- list(one_source_pro %>% filter(Tenure == 'social' & InDec21 == TRUE) %>% count(CombinedCategory, name  = 'Social sector residential Number', .drop = FALSE),
                
                one_source_pro %>% filter(Tenure == 'private residential'& InDec21 == TRUE) %>% count(CombinedCategory, name  = 'Private sector residential Number'),
                
                one_source_pro %>% filter(Tenure == 'student'& InDec21 == TRUE) %>% count(CombinedCategory, name  = 'Student accommodation Number'),
                
                one_source_pro %>% filter(Tenure == 'hotel' & InDec21 == TRUE) %>% count(CombinedCategory, name  = 'Hotels Number'),
                
                one_source_pro %>% filter(Tenure == 'public'& InDec21 == TRUE) %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
                
                one_source_pro %>% filter(InDec21 == TRUE) %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Identified by 31 December 2021` = `CombinedCategory` ) %>%
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
    `Identified by 31 December 2021`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Identified by 31 December 2021`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Identified by 31 December 2021` = "Total buildings",
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


#ACM_6_4 counts all buildings identified by 31 Dec 2022
ACM_6_4 <- list(one_source_pro %>% filter(Tenure == 'social' & InDec22 == TRUE) %>% count(CombinedCategory, name  = 'Social sector residential Number', .drop = FALSE),
                
                one_source_pro %>% filter(Tenure == 'private residential'& InDec22 == TRUE) %>% count(CombinedCategory, name  = 'Private sector residential Number'),
                
                one_source_pro %>% filter(Tenure == 'student'& InDec22 == TRUE) %>% count(CombinedCategory, name  = 'Student accommodation Number'),
                
                one_source_pro %>% filter(Tenure == 'hotel' & InDec22 == TRUE) %>% count(CombinedCategory, name  = 'Hotels Number'),
                
                one_source_pro %>% filter(Tenure == 'public'& InDec22 == TRUE) %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
                
                one_source_pro %>% filter(InDec22 == TRUE) %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Identified by 31 December 2022` = `CombinedCategory` ) %>%
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
    `Identified by 31 December 2022`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Identified by 31 December 2022`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Identified by 31 December 2022` = "Total buildings",
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


#ACM_6_5 counts all buildings identified by 31 Dec 2023
ACM_6_5 <- list(one_source_pro %>% filter(Tenure == 'social' & InDec23 == TRUE) %>% count(CombinedCategory, name  = 'Social sector residential Number', .drop = FALSE),
                
                one_source_pro %>% filter(Tenure == 'private residential'& InDec23 == TRUE) %>% count(CombinedCategory, name  = 'Private sector residential Number'),
                
                one_source_pro %>% filter(Tenure == 'student'& InDec23 == TRUE) %>% count(CombinedCategory, name  = 'Student accommodation Number'),
                
                one_source_pro %>% filter(Tenure == 'hotel' & InDec23 == TRUE) %>% count(CombinedCategory, name  = 'Hotels Number'),
                
                one_source_pro %>% filter(Tenure == 'public'& InDec23 == TRUE) %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
                
                one_source_pro %>% filter(InDec23 == TRUE) %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Identified by 31 December 2023` = `CombinedCategory` ) %>%
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
    `Identified by 31 December 2023`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Identified by 31 December 2023`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Identified by 31 December 2023` = "Total buildings",
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



#ACM_6_6 counts all buildings identified by 31 Dec 2024
ACM_6_6 <- list(one_source_pro %>% filter(Tenure == 'social' & InDec24 == TRUE) %>% count(CombinedCategory, name  = 'Social sector residential Number', .drop = FALSE),
                
                one_source_pro %>% filter(Tenure == 'private residential'& InDec24 == TRUE) %>% count(CombinedCategory, name  = 'Private sector residential Number'),
                
                one_source_pro %>% filter(Tenure == 'student'& InDec24 == TRUE) %>% count(CombinedCategory, name  = 'Student accommodation Number'),
                
                one_source_pro %>% filter(Tenure == 'hotel' & InDec24 == TRUE) %>% count(CombinedCategory, name  = 'Hotels Number'),
                
                one_source_pro %>% filter(Tenure == 'public'& InDec24 == TRUE) %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
                
                one_source_pro %>% filter(InDec24 == TRUE) %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Identified by 31 December 2024` = `CombinedCategory` ) %>%
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
    `Identified by 31 December 2024`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Identified by 31 December 2024`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Identified by 31 December 2024` = "Total buildings",
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


ACM_6_7 <- list(one_source_pro %>% filter(Tenure == 'social') %>% count(CombinedCategory, name  = 'Social sector residential Number'),
     
     one_source_pro %>% filter(Tenure == 'private residential') %>% count(CombinedCategory, name  = 'Private sector residential Number'),
     
     one_source_pro %>% filter(Tenure == 'student') %>% count(CombinedCategory, name  = 'Student accommodation Number'),
     
     one_source_pro %>% filter(Tenure == 'hotel') %>% count(CombinedCategory, name  = 'Hotels Number'),
     
     one_source_pro %>% filter(Tenure == 'public') %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number'),
     
     one_source_pro %>% count(CombinedCategory, name = 'Total: all tenures Number')) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`All buildings identified` = `CombinedCategory` ) %>%
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
    `All buildings identified`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`All buildings identified`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `All buildings identified` = "Total buildings",
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

ACM_6 <- list(ACM_6_1, ACM_6_2, ACM_6_3, ACM_6_4, ACM_6_5, ACM_6_6, ACM_6_7)