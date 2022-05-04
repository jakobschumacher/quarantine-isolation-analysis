get_numerical_results <- function(df, demographiedaten, externalinput){

  # Create empty list  
  resultslist <- list()


# Results of processing ---------------------------------------------------

  resultslist$queried <- tar_read(df_prefiltered) %>% nrow()
  
  resultslist$emptydates <- tar_read(df_prefiltered) %>% 
    count(emptydates) %>% 
    filter(emptydates == "emptydates") %>% 
    pull(n)
  
  resultslist$wrongid <- tar_read(df_prefiltered) %>% 
    filter(emptydates == "emptydates") %>% 
    count(wrongID) %>% 
    filter(wrongID == "wrongID") %>% 
    pull(n)
  
  resultslist$outofrange <- tar_read(df_prefiltered) %>% 
    filter(emptydates != "emptydates") %>% 
    filter(wrongID != "wrongID") %>% 
    count(outofrange) %>% 
    filter(outofrange == "outofrange") %>% 
    pull(n)
  
  resultslist$definitionfullfilled <- tar_read(df_prefiltered) %>% 
    filter(emptydates != "emptydates") %>% 
    filter(wrongID != "wrongID") %>% 
    filter(outofrange != "outofrange") %>% 
    nrow()
  
  resultslist$typingerror <- tar_read(df_prefiltered) %>% 
    filter(emptydates != "emptydates") %>% 
    filter(wrongID != "wrongID") %>% 
    filter(outofrange != "outofrange") %>% 
    count(typingerror) %>% 
    filter(typingerror == "typingerror") %>% 
    pull(n)
  
  # Results of deduplication
  resultslist$deleted_duplicates_table <- tar_read(df_deduplicated) %>% 
    tabyl(DatensatzKategorie) %>% 
    rename(deduplicated_df = n) %>% 
    left_join(tar_read(df_filtered) %>% 
                tabyl(DatensatzKategorie) %>% 
                rename(filtered_df = n), by = "DatensatzKategorie") %>% 
    mutate(deleted_duplicates = filtered_df - deduplicated_df) %>% 
    select(DatensatzKategorie, deleted_duplicates)
  
  resultslist$deleted_duplicates_quarantines <- resultslist$deleted_duplicates_table %>% filter(DatensatzKategorie == "Kontakt-COVID-19") %>% pull(deleted_duplicates)
  resultslist$deleted_duplicates_isolations <- resultslist$deleted_duplicates_table %>% filter(DatensatzKategorie == "COVID-19") %>% pull(deleted_duplicates)
  
  # Find number of adjusted contact person times
  resultslist$adjustedQuarantines <- tar_read(df) %>% 
    filter(overlapadjusted == "adjusted") %>% 
    nrow()
  
  # Demographic data
  resultslist$N <- demographiedaten %>% summarise(N = sum(value)) %>% pull(N)
  resultslist$N_0_6 <- demographiedaten %>% filter(name == '0 to 6') %>% pull(value)
  resultslist$N_7_17 <- demographiedaten %>% filter(name == '7 to 17') %>% pull(value)
  resultslist$N_18_64 <- demographiedaten %>% filter(name == '18 to 64') %>% pull(value)
  resultslist$N_65_110 <- demographiedaten %>% filter(name == '65 to 110') %>% pull(value)
  
  
  

# Analysis of quantity of isolation and quarantines  ----------------------


  resultslist$I_n <- df %>% filter(DatensatzKategorie == "COVID-19") %>% nrow()
  resultslist$Q_n <- df %>% filter(DatensatzKategorie == "Kontakt-COVID-19") %>% nrow()
  
  resultslist$I_p <- round(100 * resultslist$I_n / resultslist$N,1) 
  resultslist$Q_p <- round(100 * resultslist$Q_n / resultslist$N,1) 
  

  
  # total time by age group
  resultslist$totaltime_groups <- df %>% 
    group_by(DatensatzKategorie, AgeGroup) %>% 
    summarise(completeduration_days = sum(dauer), completeduration_years = sum(dauer) / 365, .groups = "drop_last") %>% 
    left_join(demographiedaten, by = c("AgeGroup" = "name")) %>% 
    mutate(percentage = round(100 * completeduration_days / sum(completeduration_days),1)) %>% 
    mutate(completeduration_person = round(completeduration_days / value,1)) %>% 
    ungroup()
  
  
  # Quarantines and isolation per person
  resultslist$QundIproPerson_table <- df %>% 
    group_by(AnonID) %>% 
    summarise(n = n()) %>% 
    count(number = n) %>% 
    mutate(p= round( n / sum(n) * 100, 1)) 
  
  resultslist$QundIproPerson_1_order_n <- resultslist$QundIproPerson_table %>% 
    filter(number==1) %>% pull(n)
  
  resultslist$QundIproPerson_1_order_p <- resultslist$QundIproPerson_table %>% 
    filter(number==1) %>% pull(p)
  
  resultslist$QundIproPerson_2_order_n <- resultslist$QundIproPerson_table %>% 
    filter(number==2) %>% pull(n)
  
  resultslist$QundIproPerson_2_order_p <- resultslist$QundIproPerson_table %>% 
    filter(number==2) %>% pull(p)
  
  resultslist$QundIproPerson_3_order_n <- resultslist$QundIproPerson_table %>% 
    filter(number==3) %>% pull(n)
  
  resultslist$QundIproPerson_3_order_p <- resultslist$QundIproPerson_table %>% 
    filter(number==3) %>% pull(p)
  
  resultslist$QundIproPerson_4_order_n <- resultslist$QundIproPerson_table %>% 
    filter(number==4) %>% pull(n)
  
  resultslist$QundIproPerson_4_order_p <- resultslist$QundIproPerson_table %>% 
    filter(number==4) %>% pull(p)
  
  resultslist$QundIproPerson_5_order_n <- resultslist$QundIproPerson_table %>% 
    filter(number==5) %>% pull(n)
  
  resultslist$QundIproPerson_5_order_p <- resultslist$QundIproPerson_table %>% 
    filter(number==5) %>% pull(p)
  
  
  
  
  

# Analysis of the duration of isolation and quarantines  ------------------

  
  # Analysis of duration of isolation
  
  # Isolation duration
  resultslist$MedianeDauerI <- df %>% filter(DatensatzKategorie == "COVID-19") %>% summarise(quint = quantile(dauer)) %>% pull()
  
  # Isolation duration by recommendation period
  resultslist$MedianeDauerI_Rec <- df %>% 
    filter(DatensatzKategorie == "COVID-19") %>% 
    group_by(I_Duration) %>% 
    summarise(quint = quantile(dauer, probs = 0.5 ), .groups = 'drop') 
  
  resultslist$MedianeDauerI_Rec_1 <- resultslist$MedianeDauerI_Rec %>% filter(I_Duration == "I_Duration_1") %>% select(quint) %>% pull()
  
  resultslist$MedianeDauerI_Rec_2 <- resultslist$MedianeDauerI_Rec %>% filter(I_Duration == "I_Duration_2") %>% select(quint) %>% pull()
  
  resultslist$MedianeDauerI_Rec_3 <- resultslist$MedianeDauerI_Rec %>% filter(I_Duration == "I_Duration_3") %>% select(quint) %>% pull()

  
  # Analysis of duration of quarantines
  
  # Quarantine duration
  resultslist$MedianeDauerQ <- df %>% 
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
    summarise(quint = quantile(dauer)) %>% 
    pull()
  
  
  # Quarantine duration by recommendation period
  resultslist$MedianeDauerQ_Rec <- df %>% 
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
    group_by(Q_Duration) %>% 
    summarise(quint = quantile(dauer, probs = 0.5 ), .groups = 'drop') 
  
  resultslist$MedianeDauerQ_Rec_1 <- resultslist$MedianeDauerQ_Rec %>% filter(Q_Duration == "Q_Duration_1") %>% select(quint) %>% pull()
  resultslist$MedianeDauerQ_Rec_2 <- resultslist$MedianeDauerQ_Rec %>% filter(Q_Duration == "Q_Duration_2") %>% select(quint) %>% pull()
  resultslist$MedianeDauerQ_Rec_3 <- resultslist$MedianeDauerQ_Rec %>% filter(Q_Duration == "Q_Duration_3") %>% select(quint) %>% pull()
  resultslist$MedianeDauerQ_Rec_4 <- resultslist$MedianeDauerQ_Rec %>% filter(Q_Duration == "Q_Duration_4") %>% select(quint) %>% pull()
  
  # Total duration of quarantine and isolation
  resultslist$qi_d <- df %>% summarise(sum(dauer)) %>% pull()
  
  # Total duration of quarantine and isolation in years
  resultslist$qi_d_in_y <- df %>% summarise(round(sum(dauer)/365)) %>% pull()
  
  # Total duration of quarantine  in years
  resultslist$q_d_in_y <- df %>%
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
    summarise(round(sum(dauer)/365)) %>% 
    pull()
  
  # Total duration of isolation  in years
  resultslist$i_d_in_y <- df %>%
    filter(DatensatzKategorie == "COVID-19") %>% 
    summarise(round(sum(dauer)/365)) %>% 
    pull()
  

# Analysis of the ratio of contact persons per case -----------------------

  
  ## Calculate ratio of quarantines to isolations
  resultslist$K_F_Verhaeltnis <- df %>%
    filter(AbsonderungVon > externalinput$StartDateKP) %>% 
    filter(AbsonderungVon < externalinput$EndDateKP) %>% 
    group_by(DatensatzKategorie) %>%
    summarise(n=n(), .groups = "drop") %>% 
    pivot_wider(names_from = DatensatzKategorie, values_from = n) %>% 
    janitor::clean_names() %>% 
    mutate(verhaeltnis = round(kontakt_covid_19 / covid_19,2)) %>% 
    pull(verhaeltnis) %>% 
    round(., digits = 2)
  
  resultslist$K_F_Verhaeltnis_QDef <- df %>%
    filter(AbsonderungVon > externalinput$StartDateKP) %>% 
    filter(AbsonderungVon < externalinput$EndDateKP) %>% 
    group_by(Q_Def, DatensatzKategorie) %>%
    summarise(n=n(), .groups = "drop") %>% 
    pivot_wider(names_from = DatensatzKategorie, values_from = n) %>% 
    janitor::clean_names() %>% 
    mutate(verhaeltnis = round(kontakt_covid_19 / covid_19,2))
  
  resultslist$K_F_Verhaeltnis_QDef_1 <- resultslist$K_F_Verhaeltnis_QDef %>% filter(q_def == "Q_Def_1") %>% pull(verhaeltnis)
  resultslist$K_F_Verhaeltnis_QDef_2 <- resultslist$K_F_Verhaeltnis_QDef %>% filter(q_def == "Q_Def_2") %>% pull(verhaeltnis)
  resultslist$K_F_Verhaeltnis_QDef_3 <- resultslist$K_F_Verhaeltnis_QDef %>% filter(q_def == "Q_Def_3") %>% pull(verhaeltnis)
  

# Analysis of isolations following quarantines ----------------------------

  
  
  # n of i after q 
  resultslist$I_after_Q <- df %>%
    filter(AbsonderungVon > externalinput$StartDateKP) %>% 
    filter(AbsonderungVon < externalinput$EndDateKP) %>% 
    filter(DatensatzKategorie == "COVID-19") %>% 
    count(result) %>% 
    pivot_wider(names_from = result, values_from = n) %>% 
    as.list()  
  
  resultslist$I_n_kptime <- df %>% 
    filter(AbsonderungVon > externalinput$StartDateKP) %>% 
    filter(AbsonderungVon < externalinput$EndDateKP) %>% 
    filter(DatensatzKategorie == "COVID-19") %>% nrow()
  
  
  # n of q with i afterwards
  resultslist$Q_with_I_after <- df %>%
    filter(AbsonderungVon > externalinput$StartDateKP) %>% 
    filter(AbsonderungVon < externalinput$EndDateKP) %>% 
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
    count(result) %>% 
    pivot_wider(names_from = result, values_from = n) %>% 
    as.list()  
  
  resultslist$Q_n_kptime <- df %>% 
    filter(AbsonderungVon > externalinput$StartDateKP) %>% 
    filter(AbsonderungVon < externalinput$EndDateKP) %>% 
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% nrow()
  
  # Calculation of R
  resultslist$r <- round(resultslist$Q_with_I_after$I_correct_after_Q / resultslist$I_n_kptime,2)
  
  
  # n of quarantine by quarantine definition
  resultslist$Q_n_by_QDef <- df %>%
    filter(AbsonderungVon > externalinput$StartDateKP) %>% 
    filter(AbsonderungVon < externalinput$EndDateKP) %>% 
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
  resultslist$Q_with_too_late_I_by_QDef_table <- df %>%
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
    mutate(percentage = round(100*n/N,1))
  
  # N and percent of quarantines with I too long afterwards by age group
  resultslist$Q_with_too_late_I_by_Agegroup_table <- df %>%
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
    group_by(AgeGroup) %>% 
    count(result) %>% 
    filter(result == "I_too_long_after_Q") %>% 
    left_join(resultslist$Q_n_by_AgeGroup %>% rename(N=n), by="AgeGroup") %>% 
    mutate(percentage = round(100*n/N,1))
  

# Analysis of timeliness --------------------------------------------------
  
  resultslist$q_timeliness_median <- df %>%
    filter(DatensatzKategorie == "Kontakt-COVID-19") %>%
    filter(Q_Duration == "Q_Duration_1" | Q_Duration == "Q_Duration_3") %>% 
    mutate(Q_timeliness = 14 - dauer - 1) %>% 
    filter(Q_timeliness >= 0) %>% 
    summarise(quant = quantile(Q_timeliness)) %>% 
    pull()
    
  


# Generate table 2 -----------------------------------------------------------------

# ├ Age group table ----
  
  resultslist$agegroup_table <- 
    demographiedaten %>% 
    # add number of quarnatines
    rename(N = value, AgeGroup = name) %>% 
    left_join(df %>% 
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                count(AgeGroup) %>% 
                rename(q_n = n), by = "AgeGroup") %>% 
    # add number of isolations
    left_join(df %>% 
                filter(DatensatzKategorie == "COVID-19") %>% 
                count(AgeGroup) %>% 
                rename(i_n = n), by = "AgeGroup") %>% 
    # add percentage of quarantines
    mutate(q_p = round(100 * q_n / N,1)) %>% 
    # add percentage of isolations
    mutate(i_p = round(100 * i_n / N,1)) %>% 
    # add duration of quarantines
    left_join(df %>% 
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                group_by(AgeGroup) %>% 
                summarise(q_d = round(mean(dauer),1)), by = "AgeGroup") %>% 
    # add duration of isolations
    left_join(df %>% 
                filter(DatensatzKategorie == "COVID-19") %>% 
                group_by(AgeGroup) %>% 
                summarise(i_d = round(mean(dauer),1)), by = "AgeGroup") %>% 
    # add sum of quarantine days
    left_join(df %>% 
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                group_by(AgeGroup) %>% 
                summarise(q_sum_in_y = round(sum(dauer)/365,1)), by = "AgeGroup") %>% 
    # add sum of isolation days
    left_join(df %>% 
                filter(DatensatzKategorie == "COVID-19") %>% 
                group_by(AgeGroup) %>% 
                summarise(i_sum_in_y = round(sum(dauer)/365,1)), by = "AgeGroup") %>% 
    # quarantine days per person
    mutate(q_sum_in_d_per_p = round(q_sum_in_y*365/N,1)) %>%     
    # quarantine days per person
    mutate(i_sum_in_d_per_p = round(i_sum_in_y*365/N,1)) %>% 
    # contained cases
    left_join(resultslist$Q_with_correct_I_by_Agegroup_table %>% 
                select(AgeGroup, contained = n), by = "AgeGroup") %>% 
    left_join(resultslist$Q_with_correct_I_by_Agegroup_table %>% 
                select(AgeGroup, containedp = percentage), by = "AgeGroup") %>% 
  # non-contained cases
    left_join(resultslist$Q_with_too_late_I_by_Agegroup_table %>% 
                select(AgeGroup, toolate = n), by = "AgeGroup") %>% 
  left_join(resultslist$Q_with_too_late_I_by_Agegroup_table %>% 
              select(AgeGroup, toolatep = percentage), by = "AgeGroup")
  

# ├ Q_Def table -------------------------------------------------------------

resultslist$qdef_table <- df %>%
    select(Q_Def) %>% 
    distinct() %>% 
    # add number of quarantines
    left_join(df %>% 
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                count(Q_Def) %>% 
                rename(q_n = n), by = "Q_Def") %>% 
    # add number of isolations
    left_join(df %>% 
                filter(DatensatzKategorie == "COVID-19") %>% 
                count(Q_Def) %>% 
                rename(i_n = n), by = "Q_Def") %>% 
    # add duration of quarantines
    left_join(df %>% 
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                group_by(Q_Def) %>% 
                summarise(q_d = round(mean(dauer),1)), by = "Q_Def") %>% 
    # add duration of isolations
    left_join(df %>% 
                filter(DatensatzKategorie == "COVID-19") %>% 
                group_by(Q_Def) %>% 
                summarise(i_d = round(mean(dauer),1)), by = "Q_Def") %>% 
    # add sum of quarantine days
    left_join(df %>% 
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                group_by(Q_Def) %>% 
                summarise(q_sum_in_y = round(sum(dauer)/365,1)), by = "Q_Def") %>% 
    # add sum of isolation days
    left_join(df %>% 
                filter(DatensatzKategorie == "COVID-19") %>% 
                group_by(Q_Def) %>% 
                summarise(i_sum_in_y = round(sum(dauer)/365,1)), by = "Q_Def") %>% 
    # contained cases
    left_join(resultslist$Q_with_correct_I_by_QDef_table %>% 
                select(Q_Def, contained = n), by = "Q_Def") %>% 
    left_join(resultslist$Q_with_correct_I_by_QDef_table %>% 
                select(Q_Def, containedp = percentage), by = "Q_Def") %>% 
    # non-contained cases
    left_join(resultslist$Q_with_too_late_I_by_QDef_table %>% 
                select(Q_Def, toolate = n), by = "Q_Def") %>% 
    left_join(resultslist$Q_with_too_late_I_by_QDef_table %>% 
                select(Q_Def, toolatep = percentage), by = "Q_Def")
  

# ├ Total table ----  
  
  resultslist$total_table <-  demographiedaten %>% 
    # add number of quarnatines
    summarise(N = sum(value)) %>% 
    mutate(total = "total") %>% 
    bind_cols(df %>% 
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                count() %>% 
                rename(q_n = n)) %>% 
    # add number of isolations
    bind_cols(df %>% 
                filter(DatensatzKategorie == "COVID-19") %>% 
                count() %>% 
                rename(i_n = n)) %>% 
    # add percentage of quarantines
    mutate(q_p = round(100 * q_n / N,1)) %>% 
    # add percentage of isolations
    mutate(i_p = round(100 * i_n / N,1)) %>% 
    # add duration of quarantines
    bind_cols(df %>% 
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                group_by() %>% 
                summarise(q_d = round(mean(dauer),1))) %>% 
    # add duration of isolations
    bind_cols(df %>% 
                filter(DatensatzKategorie == "COVID-19") %>% 
                group_by() %>% 
                summarise(i_d = round(mean(dauer),1))) %>% 
    # add sum of quarantine days
    bind_cols(df %>% 
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                group_by() %>% 
                summarise(q_sum_in_y = round(sum(dauer)/365,1))) %>% 
    # add sum of isolation days
    bind_cols(df %>% 
                filter(DatensatzKategorie == "COVID-19") %>% 
                group_by() %>% 
                summarise(i_sum_in_y = round(sum(dauer)/365,1))) %>% 
    # quarantine days per person
    mutate(q_sum_in_d_per_p = round(q_sum_in_y*365/N,1)) %>%     
    # # quarantine days per person
    mutate(i_sum_in_d_per_p = round(i_sum_in_y*365/N,1)) %>% 
    # contained cases
    bind_cols(df %>%
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                count(result) %>% 
                filter(result == "I_correct_after_Q") %>% 
                bind_cols(N = resultslist$N) %>% 
                mutate(percentage = round(100*n/N,1)) %>% 
                select(contained = n, containedp = percentage)
    ) %>% 
    # non-contained cases
    bind_cols(df %>%
                filter(DatensatzKategorie == "Kontakt-COVID-19") %>% 
                count(result) %>% 
                filter(result == "I_too_long_after_Q") %>% 
                bind_cols(N = resultslist$N) %>% 
                mutate(percentage = round(100*n/N,1)) %>% 
                select(toolate = n)
    ) 
  

# End of function ---------------------------------------------------------


  resultslist
  
}



