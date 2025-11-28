#ACM_3 does not include buildings which have completed remediation
one_source_pro_vacants <- one_source_pro %>% filter(CombinedCategory != 'Completed Remediation')

#creating a new CombinedCategory variable which has the suffix - Vacant in the row names
one_source_pro_vacants$CombinedCategory_Vacants <- one_source_pro_vacants$CombinedCategory

levels(one_source_pro_vacants$CombinedCategory_Vacants) <- c('Completed Remediation', 'Works complete awaiting building control signoff - Vacant', 'Remediation started - cladding removed - Vacant', 'Remediation started - Vacant', 'Remediation plans in place - Vacant'
                                             , 'Reported an intent to remediate - Vacant', 'Remediation plan unclear - Vacant')


#ACM_3_1 shows the remediation status and tenure of vacant buildings
ACM_3_1 <- list(one_source_pro_vacants %>% filter(Tenure == 'social' & grepl('Vacant', Vacant) ) %>% count(CombinedCategory_Vacants, name  = 'Social sector residential', .drop = FALSE),
              
                one_source_pro_vacants %>% filter(Tenure == 'private residential'& grepl('Vacant', Vacant)) %>% count(CombinedCategory_Vacants, name  = 'Private sector residential'),
              
                one_source_pro_vacants %>% filter(Tenure == 'student' & grepl('Vacant', Vacant)) %>% count(CombinedCategory_Vacants, name  = 'Student accommodation'),
              
                one_source_pro_vacants %>% filter(Tenure == 'hotels' & grepl('Vacant', Vacant)) %>% count(CombinedCategory_Vacants, name  = 'Hotels'),
              
                one_source_pro_vacants %>% filter(Tenure == 'public' & grepl('Vacant', Vacant)) %>% count(CombinedCategory_Vacants, name  = 'Publicly-owned buildings')) %>% 
  reduce(full_join, by = 'CombinedCategory_Vacants') %>%
  rename(`Remediation Category` = `CombinedCategory_Vacants` ) %>%
  
  #replace NA with 0
  replace(is.na(.), 0) %>%
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Remediation Category`) %>% 
  
  #creates the total row
  bind_rows(
    summarise(.,
              `Remediation Category` = "Total buildings",
              `Social sector residential` = sum(`Social sector residential`),
              `Private sector residential` = sum(`Private sector residential`),
              `Student accommodation` = sum(`Student accommodation`),
              `Hotels` = sum(`Hotels`),
              `Publicly-owned buildings` = sum(`Publicly-owned buildings`),
    )
  )

