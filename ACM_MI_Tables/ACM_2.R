library(forcats)

one_source_pro <- one_source_pro %>%
  mutate(Tenure = factor(Tenure),
         CombinedCategory = factor(CombinedCategory, levels = c('Remediation completed', 'Works complete awaiting building control signoff', 'Remediation started - cladding removed',
                                                                              'Remediation started', 'Plans in place', 'Responded with intent', 'Remediation plan unclear') ))

levels(one_source_pro$CombinedCategory) <- c('Completed Remediation', 'Works complete awaiting building control signoff', 'Remediation started - cladding removed', 'Remediation started', 'Remediation plans in place'
                             , 'Reported an intent to remediate', 'Remediation plan unclear')


ACM_2 <- list(one_source_pro %>% filter(Tenure == 'social') %>% count(CombinedCategory, name  = 'Social sector residential Number', sort = FALSE),
                         
                         one_source_pro %>% filter(Tenure == 'private residential') %>% count(CombinedCategory, name  = 'Private sector residential Number', sort = FALSE),
              
                         one_source_pro %>% filter(Tenure == 'student') %>% count(CombinedCategory, name  = 'Student accommodation Number', sort = FALSE),
              
                         one_source_pro %>% filter(Tenure == 'hotels') %>% count(CombinedCategory, name  = 'Hotels Number', sort = FALSE),
              
                         one_source_pro %>% filter(Tenure == 'public') %>% count(CombinedCategory, name  = 'Publicly-owned buildings Number', sort = FALSE),
              
                         one_source_pro %>% count(CombinedCategory, name = 'Total: all tenures Number', sort = FALSE)) %>% reduce(full_join, by = 'CombinedCategory') %>%
  rename(`Remediation Category` =`CombinedCategory` ) %>%
  #calculates the percentage of the total
  mutate(
    `Social sector residential Percentage (%)` = (`Social sector residential Number` / sum(`Social sector residential Number`)),
    `Private sector residential Percentage (%)` = (`Private sector residential Number` / sum(`Private sector residential Number`)),
    `Student accommodation Percentage (%)` = (`Student accommodation Number` / sum(`Student accommodation Number`)),
    `Hotels Percentage (%)` = (`Hotels Number` / sum(`Hotels Number`)),
    `Publicly-owned buildings Percentage (%)` = (`Publicly-owned buildings Number` / sum(`Publicly-owned buildings Number`)),
    `Total: all tenures Percentage (%)` = (`Total: all tenures Number` / sum(`Total: all tenures Number`))) %>%
  #reorders the columns
  select(
    `Remediation Category`,
    `Social sector residential Number`, `Social sector residential Percentage (%)`,
    `Private sector residential Number`, `Private sector residential Percentage (%)`,
    `Student accommodation Number`, `Student accommodation Percentage (%)`,
    `Hotels Number`, `Hotels Percentage (%)`,
    `Publicly-owned buildings Number`, `Publicly-owned buildings Percentage (%)`,
    `Total: all tenures Number`, `Total: all tenures Percentage (%)`
  ) %>%
  replace(is.na(.), 0) %>%
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
  ) %>%
  arrange(`Remediation Category`)

print(ACM_2$`Remediation Category`)
