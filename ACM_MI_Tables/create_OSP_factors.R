#this script sets up factor variables so count() will work correctly across scripts

#combined category is set to display in the correct order
one_source_pro <- one_source_pro %>%
  mutate(Tenure = factor(Tenure),
         CombinedCategory = factor(CombinedCategory, levels = c('Remediation completed', 'Works complete awaiting building control signoff', 'Remediation started - cladding removed',
                                                                'Remediation started', 'Plans in place', 'Responded with intent', 'Remediation plan unclear') ))

one_source_pro_last_month <- one_source_pro_last_month %>%
  mutate(Tenure = factor(Tenure)) %>%
  rename(`Tenure Last Month` = `Tenure`)