#these three schemes count as government funded for private residential buildings
government_funded <- c('private-fund', 'bsf', 'css')

#ACM 8 shows remediation progress for private residential and social buildings, by sources of funding
ACM_8 <- list(one_source_pro %>% filter(Tenure == 'social' & Payer == 'social-fund') %>% count(CombinedCategory, name  = 'In social fund [note 4]', .drop = FALSE),
              
              one_source_pro %>% filter(Tenure == 'social' & Payer != 'social-fund') %>% count(CombinedCategory, name  = 'Not in social fund'),
              
              one_source_pro %>% filter(Tenure == 'private residential' & Payer %in% government_funded) %>% count(CombinedCategory, name  = 'Private: government funded [note 5]'),
              
                one_source_pro %>% filter(Tenure == 'private residential' & ! Payer %in% government_funded) %>% count(CombinedCategory, name  = 'Private: not government funded')) %>%

  reduce(full_join, by = 'CombinedCategory') %>%
  
  rename(`Remediation Category` = `CombinedCategory` ) %>%
  
  #replace NA with 0
  replace(is.na(.), 0) %>%
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Remediation Category`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Remediation Category` = "Total ",
              `In social fund [note 4]` = sum(`In social fund [note 4]`),
              `Not in social fund` = sum(`Not in social fund`),
              `Private: government funded [note 5]` = sum(`Private: government funded [note 5]`),
              `Private: not government funded` = sum(`Private: not government funded`),
    )
  )
