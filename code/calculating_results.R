get_numerical_results <- function(df, demographiedaten, externalinput){

  # resultslist <- list()

  # Get stored results
  resultslist <- c(
    readRDS("data/results/info_about_deduplication.rds")
  )  
  
  #####################################################
  # Results of filtering
  #####################################################
  rows_to_be_filtered <- tar_read(rows_to_be_filtered)
  resultslist$queried <- nrow(rows_to_be_filtered)  
  resultslist$definitionvar <- rows_to_be_filtered %>% 
    select(definitionvar) %>% 
    table()
  resultslist$outofrange <- rows_to_be_filtered %>% 
    filter(definitionvar == "defined") %>% 
    select(outofrange) %>% 
    table()
  resultslist$wrongEntries <- rows_to_be_filtered %>% 
    filter(definitionvar == "defined") %>% 
    filter(outofrange == "inrange") %>% 
    select(filtervar) %>% 
    table()  
  
 #####################################################
  #Numbers of quarantine and isolation
  #####################################################

  # Total Population
  resultslist$N <- demographiedaten %>% summarise(N = sum(value))
  
  # N quarantines
  resultslist$AnzahlQAgegroup <- df %>% filter(DatensatzKategorie == "Kontakt-COVID-19") %>% group_by(AgeGroup) %>% summarise(n = n()) %>% pull(n)
  resultslist$GesamtQ <- sum(resultslist$AnzahlQAgegroup) 
  
  # N isolations
  resultslist$AnzahlIAgegroup <- df %>% filter(DatensatzKategorie == "COVID-19") %>%  group_by(AgeGroup) %>% summarise(n = n()) %>% pull(n)
  resultslist$GesamtI <- sum(resultslist$AnzahlIAgegroup )
  
  # N quarantines by anonID
  resultslist$PercentQ_byAnonID <- df %>% filter(DatensatzKategorie == "Kontakt-COVID-19") %>%  group_by(AnonID) %>% summarise(n = n(), .groups = "drop")   %>% summarise(n = n()) %>% mutate(resultslist$N) %>% mutate(percentage = round(100*n/N))
  
  # N isolations by anonID
  resultslist$PercentI_byAnonID <- df %>% filter(DatensatzKategorie == "COVID-19") %>%  group_by(AnonID) %>% summarise(n = n(), .groups = "drop")   %>% summarise(n = n()) %>% mutate(resultslist$N) %>% mutate(percentage = round(100*n/N))
  
  # Quarantines by population
  resultslist$AnzahlQproBev <- round(100*resultslist$GesamtQ / demographiedaten %>% summarise(sum(value)) %>% pull(), 1)
  resultslist$AnzahlQproBevAgegroup <- round(100 * resultslist$AnzahlQAgegroup  / demographiedaten$value, 1)
  
  # Isolations by population
  resultslist$AnzahlIproBev <- round(100*resultslist$GesamtI / demographiedaten %>% summarise(sum(value)) %>% pull(),1)
  resultslist$AnzahlIproBevAgegroup <- round(100 * resultslist$AnzahlIAgegroup  / demographiedaten$value, 1)
  
  # Quarantines and isolation per person
  resultslist$QundIproPerson <- df %>% 
    group_by(AnonID) %>% 
    summarise(n = n()) %>% 
    count(number = n) %>% 
    mutate(Percentage= round( n / sum(n) * 100, 1)) 
  
  # Highest number of quarantine and isolation per person
  resultslist$highestQundIproPerson <- df %>% group_by(AnonID) %>% 
    summarise(n = n()) %>%
    count(number = n) %>% 
    slice_max(number) %>% 
    pull(number)
  
  
  # Quarantines and isolation by population per age group
  resultslist$incidence <- df %>% 
    group_by(AgeGroup, DatensatzKategorie) %>% 
    summarise(n = n(), .groups = 'drop') %>% 
    left_join(demographiedaten, by= c("AgeGroup" = "name") ) %>% 
    rename(N = value) %>% 
    mutate(incidence = round(100*n/N)) %>% 
    mutate(DatensatzKategorie = recode_factor(DatensatzKategorie, "COVID-19" = "isolation", "Kontakt-COVID-19" = "quarantine"))
  
  # Quarantines and isolation by population
  resultslist$incidence_total <- df %>% 
    group_by(DatensatzKategorie) %>% 
    summarise(n = n(), .groups = 'drop') %>% 
    mutate(totalpopulation = demographiedaten %>% summarise(sum(value))) %>% 
    mutate(incidence = round(100*n/totalpopulation)) 
  
  
  
  
  ###################################################
  # Succes measurement
  ###################################################
  
  # n of i after q 
  resultslist$I_after_Q <- df %>%
    filter(DatensatzKategorie == "COVID-19") %>% 
    count(result) %>% 
    pivot_wider(names_from = result, values_from = n) %>% 
    as.list()  
  
  # n of q with i afterwards
  resultslist$Q_with_I_after <- df %>%
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
    count(result) %>% 
    pivot_wider(names_from = result, values_from = n) %>% 
    as.list()  
  
  # n of quarantine by quarantine definition
  resultslist$Q_n_by_QDef <- df %>%
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>%
    count(Q_Def)
  
  # n and percent of quarantines with correct I afterwards by quarantine defintion
  resultslist$Q_with_correct_I_by_QDef_table <- df %>%
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
    group_by(Q_Def) %>% 
    count(result) %>% 
    filter(result == "I_correct_after_Q") %>% 
    left_join(resultslist$Q_n_by_QDef %>% rename(N=n), by="Q_Def") %>% 
    mutate(percentage = round(100*n/N))
  
  # n and percent of quarantines with correct I afterwards by quarantine defintion
  resultslist$Q_with_correct_I_by_QDef_table <- df %>%
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
    group_by(Q_Def) %>% 
    count(result) %>% 
    filter(result == "I_too_long_after_Q") %>% 
    left_join(resultslist$Q_n_by_QDef %>% rename(N=n), by="Q_Def") %>% 
    mutate(percentage = round(100*n/N,1))
  
  # n of quarantine by age group
  resultslist$Q_n_by_AgeGroup <- df %>%
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>%
    count(AgeGroup)
  
  # N and percent of quarantines with correct I by age group
  resultslist$Q_with_correct_I_by_Agegroup_table <- df %>%
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
    group_by(AgeGroup) %>% 
    count(result) %>% 
    filter(result == "I_correct_after_Q") %>% 
    left_join(resultslist$Q_n_by_AgeGroup %>% rename(N=n), by="AgeGroup") %>% 
    mutate(percentage = round(100*n/N))
  
  # N and percent of quarantines with I too long afterwards by age group
  resultslist$Q_with_too_late_I_by_Agegroup_table <- df %>%
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
    group_by(AgeGroup) %>% 
    count(result) %>% 
    filter(result == "I_too_long_after_Q") %>% 
    left_join(resultslist$Q_n_by_AgeGroup %>% rename(N=n), by="AgeGroup") %>% 
    mutate(percentage = round(100*n/N))
  
  
  # Find number of adjusted contact person times
  resultslist$adjustedQuarantines <- df %>% 
    filter(overlapadjusted == "adjusted") %>% 
    nrow()
  
  
  ## Calculate ratio of quarantines to isolations
  resultslist$K_F_Verhaeltnis <- df %>%
    filter(AbsonderungVon > externalinput$StartDateKP) %>% 
    filter(AbsonderungVon < externalinput$EndDateKP) %>% 
    group_by(DatensatzKategorie) %>%
    summarise(n=n(), .groups = "drop") %>% 
    pivot_wider(names_from = DatensatzKategorie, values_from = n) %>% 
    janitor::clean_names() %>% 
    mutate(verhaeltnis = round(kontakt_covid_19 / covid_19,2)) %>% 
    pull(verhaeltnis)

resultslist$K_F_Verhaeltnis_QDef <- df %>%
  filter(AbsonderungVon > externalinput$StartDateKP) %>% 
  filter(AbsonderungVon < externalinput$EndDateKP) %>% 
  group_by(Q_Def, DatensatzKategorie) %>%
  summarise(n=n(), .groups = "drop") %>% 
  pivot_wider(names_from = DatensatzKategorie, values_from = n) %>% 
  janitor::clean_names() %>% 
  mutate(verhaeltnis = round(kontakt_covid_19 / covid_19,2))


# Calculate time periods
# Total duration of quarantine and isolation
resultslist$NQundI <- df %>% summarise(sum(dauer)) %>% pull()

# Quarantine duration
resultslist$MedianeDauerQ <- df %>% filter(DatensatzKategorie == "Kontakt-COVID-19") %>% summarise(quint = quantile(dauer)) %>% pull()
resultslist$MedianeDauerQ_Rec <- df %>% 
  filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
  group_by(Q_Duration) %>% 
  summarise(quint = quantile(dauer, probs = 0.5 ), .groups = 'drop') 

# Isolation duration
resultslist$MedianeDauerI <- df %>% filter(DatensatzKategorie == "COVID-19") %>% summarise(quint = quantile(dauer)) %>% pull()
resultslist$MedianeDauerI_Rec <- df %>% 
  filter(DatensatzKategorie == "COVID-19") %>% 
  group_by(I_Duration) %>% 
  summarise(quint = quantile(dauer, probs = 0.5 ), .groups = 'drop') 

resultslist$MedianeDauerAbsonderungAgeGroup <- df %>% group_by(DatensatzKategorie, AgeGroup) %>% summarise(quint = quantile(dauer), .groups = 'drop')


# total time by age group
resultslist$totaltime_groups <- df %>% 
  group_by(DatensatzKategorie, AgeGroup) %>% 
  summarise(completeduration_days = sum(dauer), completeduration_years = sum(dauer) / 365, .groups = "drop_last") %>% 
  left_join(demographiedaten, by = c("AgeGroup" = "name")) %>% 
  mutate(percentage = round(100 * completeduration_days / sum(completeduration_days),1)) %>% 
  mutate(completeduration_person = round(completeduration_days / value,1)) %>% 
  ungroup()

# total time
resultslist$totaltime <- df %>% 
  group_by(DatensatzKategorie) %>% 
  summarise(completeduration_days = sum(dauer), completeduration_years = sum(dauer) / 365, .groups = "drop_last") %>%
  mutate(N = demographiedaten %>% summarise(sum(value))) %>% 
  mutate(completeduration_person = round(completeduration_days / N ,1)) %>% 
  ungroup()

}

