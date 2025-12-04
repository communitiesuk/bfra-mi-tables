#ACM 9 shows application progress for buildings funded by the private sector cladding remediation fund, inside and outside of london
ACM_9 <- list(one_source_pro %>% filter(PrivateFundDP == 'GLA') %>% count(PrivateFundStatusDR, name  = 'Number of buildings inside London', .drop = FALSE),
              
              one_source_pro %>% filter(PrivateFundDP != 'GLA') %>% count(PrivateFundStatusDR, name  = 'Number of buildings outside London', .drop = FALSE)) %>%
  
  reduce(full_join, by = 'PrivateFundStatusDR') %>%
  
  rename(`Category` = `PrivateFundStatusDR` ) %>%
  
  #order the rows correctly (according to the order specified in the factor levels)
  arrange(`Category`) %>% 
  
  #create the total row
  bind_rows(
    summarise(.,
              `Category` = "Total",
              `Number of buildings inside London` = sum(`Number of buildings inside London`),
              `Number of buildings outside London` = sum(`Number of buildings outside London`),
    )
  )
