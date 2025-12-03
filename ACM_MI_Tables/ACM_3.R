#ACM_3 does not include buildings which have completed remediation
one_source_pro_vacants <- one_source_pro %>% filter(CombinedCategory != 'Completed Remediation')

#remove 'Completed Remediation' as a level as it's not needed
one_source_pro_vacants <- droplevels(one_source_pro_vacants)

#creating a new CombinedCategory variable which has the suffix - Vacant in the row names
one_source_pro_vacants$CombinedCategory_Vacants <- one_source_pro_vacants$CombinedCategory

levels(one_source_pro_vacants$CombinedCategory_Vacants) <- c('Works complete awaiting building control signoff - Vacant', 'Remediation started - cladding removed - Vacant', 'Remediation started - Vacant', 'Remediation plans in place - Vacant'
                                             , 'Reported an intent to remediate - Vacant', 'Remediation plan unclear - Vacant')


#ACM_3_1 shows the remediation status and tenure of vacant buildings
ACM_3_1 <- list(one_source_pro_vacants %>% filter(Tenure == 'social' & grepl('Vacant', Vacant) ) %>% count(CombinedCategory_Vacants, name  = 'Social sector residential', .drop = FALSE),
              
                one_source_pro_vacants %>% filter(Tenure == 'private residential'& grepl('Vacant', Vacant)) %>% count(CombinedCategory_Vacants, name  = 'Private sector residential'),
              
                one_source_pro_vacants %>% filter(Tenure == 'student' & grepl('Vacant', Vacant)) %>% count(CombinedCategory_Vacants, name  = 'Student accommodation'),
              
                one_source_pro_vacants %>% filter(Tenure == 'hotel' & grepl('Vacant', Vacant)) %>% count(CombinedCategory_Vacants, name  = 'Hotels'),
              
                one_source_pro_vacants %>% filter(Tenure == 'public' & grepl('Vacant', Vacant)) %>% count(CombinedCategory_Vacants, name  = 'Publicly-owned buildings')) %>% 
  reduce(full_join, by = 'CombinedCategory_Vacants') %>%
  rename(`Remediation Category` = `CombinedCategory_Vacants` ) %>%
  
  #replace NA with 0
  replace(is.na(.), 0) %>%
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Remediation Category`) %>% 
  
  #create the total row
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

#creating ACM_3_2, showing remediation status and tenure for in-progress non vacant buildings
ACM_3_2 <- list(one_source_pro_vacants %>% filter(Tenure == 'social' & is.na(Vacant)) %>% count(CombinedCategory, name  = 'Social sector residential'),
              
                one_source_pro_vacants %>% filter(Tenure == 'private residential'& is.na(Vacant)) %>% count(CombinedCategory, name  = 'Private sector residential'),
              
                one_source_pro_vacants %>% filter(Tenure == 'student' & is.na(Vacant)) %>% count(CombinedCategory, name  = 'Student accommodation'),
              
                one_source_pro_vacants %>% filter(Tenure == 'hotel' & is.na(Vacant)) %>% count(CombinedCategory, name  = 'Hotels'),
              
                one_source_pro_vacants %>% filter(Tenure == 'public' & is.na(Vacant)) %>% count(CombinedCategory, name  = 'Publicly-owned buildings')) %>% 
  reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Remediation Category for occupied buildings` = `CombinedCategory` ) %>%
  
  #replace NA with 0
  replace(is.na(.), 0) %>%
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Remediation Category for occupied buildings`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Remediation Category for occupied buildings` = "Total buildings",
              `Social sector residential` = sum(`Social sector residential`),
              `Private sector residential` = sum(`Private sector residential`),
              `Student accommodation` = sum(`Student accommodation`),
              `Hotels` = sum(`Hotels`),
              `Publicly-owned buildings` = sum(`Publicly-owned buildings`),
    )
  )

ACM_3 <- list(ACM_3_1, ACM_3_2)
