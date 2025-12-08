#renames the levels to match what's displayed in the MI tables

levels(css_data$`Portfolio Stage`) <- c('Works not started', 'Live application', 'Works completed', 'Works started')

#reorders the levels to display properly in CSS_1_2
css_data <- css_data %>%
  mutate(`Portfolio Stage` = factor(`Portfolio Stage`, levels = c('Live application', 'Works not started', 'Works started', 'Works completed')))
