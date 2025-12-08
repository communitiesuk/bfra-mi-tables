# CSS_1_1 <- css_data %>% filter(`Scheme Origin Name` == 'Cladding Safety Scheme' & `Portfolio Stage` == 'Live application') %>% count(`Portfolio Stage`, name = 'Number') %>%
#   



#CSS_1_2 shows eligible buildings by portfolio stage
CSS_1_2 <-
  css_data %>% filter(`Portfolio Stage` != 'Live application') %>% count(`Portfolio Stage`, name = 'Number')%>%
    mutate(
      `Percentage (%)` = (`Number`/sum(`Number`))) %>%
  rename(`Eligible buildings [note_13]` =`Portfolio Stage` ) %>%
  #create total row
  bind_rows(
    summarise(.,
              `Eligible buildings [note_13]` = "Total eligible buildings",
              `Number` = sum(`Number`),
              `Percentage (%)` = sum(`Percentage (%)`)
    )
  )
