#only the listed LAs with enough high rise buildings are included in ACM_4, so that buildings can't be identified

LA_list<- c('Barking and Dagenham', 'Barnet', 'Bedford', 'Birmingham', 'Bournemouth, Christchurch and Poole','Bradford','Brent', 'Brighton and Hove', 'Bristol, City of', 'Bromley', 'Calderdale', 'Camden',
            'City of London', 'Croydon', 'Dacorum', 'Doncaster', 'Ealing', 'Elmbridge', 'Gateshead','Greenwich','Hackney', 'Hammersmith and Fulham','Haringey', 'Harrow', 'Havering', 'Hillingdon', 'Hounslow',
            'Islington', 'Kensington and Chelsea', 'Kingston upon Thames', 'Kirklees', 'Lambeth', 'Leeds', 'Leicester', 'Lewisham', 'Lincoln', 'Liverpool', 'Luton', 'Manchester', 'Medway', 'Merton', 'Milton Keynes'
            , 'Newcastle upon Tyne', 'Newham', 'Norwich', 'Nottingham', 'Oldham', 'Plymouth', 'Portsmouth', 'Reading', 'Redbridge', 'Salford', 'Sandwell', 'Sefton', 'Sheffield', 'Slough', 'Southwark', 'Stockton-on-Tees',
            'Sunderland', 'Sutton', 'Tower Hamlets', 'Trafford', 'Waltham Forest', 'Wandsworth', 'Westminster', 'Windsor and Maidenhead', 'Wolverhampton')

one_source_pro_LAs <- one_source_pro %>% filter(LocalAuthority %in% LA_list)

ACM_4 <- list(one_source_pro_LAs %>% count(LocalAuthority, name = 'Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)'),
              
              one_source_pro_LAs %>% filter(Category == 'Remediation completed') %>% count(LocalAuthority, name  = 'Completed'),
              
              one_source_pro_LAs %>% filter(Category %in% c('Remediation completed', 'Remediation started')) %>% count(LocalAuthority, name  = 'Started or completed'),

              one_source_pro_LAs %>% filter(Category != 'Remediation completed') %>% count(LocalAuthority, name  = 'Number of buildings with ACM cladding systems (unlikely to meet Building Regulations) yet to be remediated')) %>% 
  reduce(full_join, by = 'LocalAuthority') %>% 
  
  rename(`Local Authority [note_2]` = `LocalAuthority` ) %>%
  
  replace(is.na(.), 0) %>%
  #calculates the percentages needed and bands them following the tempate used in the MI tables
  mutate(
    `Percentage of buildings with ACM cladding systems completed remediation` =  (`Completed` / `Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)`),
    `Percentage of buildings with ACM cladding systems started or completed remediation` =  (`Started or completed` / `Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)`),
    
    
    `Percentage of buildings with ACM cladding systems completed remediation` = case_when(
      `Percentage of buildings with ACM cladding systems completed remediation` < 0.5 ~ 'Less than 50%',
      `Percentage of buildings with ACM cladding systems completed remediation` > 0.49 & `Percentage of buildings with ACM cladding systems completed remediation` < 1 ~ 'Between 50% and 99%',
      `Percentage of buildings with ACM cladding systems completed remediation` == 1 ~ '100%',
  ), 
  
  
  `Percentage of buildings with ACM cladding systems started or completed remediation` = case_when(
    `Percentage of buildings with ACM cladding systems started or completed remediation` < 0.5 ~ 'Less than 50%',
    `Percentage of buildings with ACM cladding systems started or completed remediation` > 0.49 & `Percentage of buildings with ACM cladding systems started or completed remediation` < 1 ~ 'Between 50% and 99%',
    `Percentage of buildings with ACM cladding systems started or completed remediation` == 1 ~ '100%',
  ),
    
    `Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)` = case_when(
      `Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)` <= 5 ~ '1 to 5',
      `Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)` > 5 & `Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)` < 11 ~ '6 to 10',
      `Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)` > 10 & `Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)` < 21 ~ '11 to 20',
      `Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)` > 20 ~ 'Over 20',
    ),
    
    `Number of buildings with ACM cladding systems (unlikely to meet Building Regulations) yet to be remediated` = case_when(
      `Number of buildings with ACM cladding systems (unlikely to meet Building Regulations) yet to be remediated` < 1 ~ 'None',
      `Number of buildings with ACM cladding systems (unlikely to meet Building Regulations) yet to be remediated` > 0 & `Number of buildings with ACM cladding systems (unlikely to meet Building Regulations) yet to be remediated` < 6 ~ '1 to 5',
      `Number of buildings with ACM cladding systems (unlikely to meet Building Regulations) yet to be remediated` > 5 & `Number of buildings with ACM cladding systems (unlikely to meet Building Regulations) yet to be remediated` < 11 ~ '6 to 10',
      `Number of buildings with ACM cladding systems (unlikely to meet Building Regulations) yet to be remediated` > 10 & `Number of buildings with ACM cladding systems (unlikely to meet Building Regulations) yet to be remediated` < 21 ~ '11 to 20'
    )
  ) %>%
  #reorders the columns
  select(`Local Authority [note_2]`, `Number of buildings identified with ACM cladding systems (unlikely to meet Building Regulations)`,
    `Number of buildings with ACM cladding systems (unlikely to meet Building Regulations) yet to be remediated`, 
    `Percentage of buildings with ACM cladding systems completed remediation`,
    `Percentage of buildings with ACM cladding systems started or completed remediation`
  ) 















