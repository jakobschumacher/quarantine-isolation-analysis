create_pairslist <- function(maximumnumber = 84){
  # This function creates a list of numbers which is needed for the other functions
  overlapcheck_pairs <- function(highest = 10){
    mytibble <- tibble(value = 1:highest) %>% 
      expand(value, value1 = value) %>% 
      filter(value < value1) 
    mylist <- map(1:nrow(mytibble), ~c(mytibble$value[.x], mytibble$value1[.x]))
    mylist
  }
  
  pairslist <- map(1:maximumnumber, ~overlapcheck_pairs(.x)) # Legt die Pairsliste an
  pairslist
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# De duplication 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This function checks wheter two entries overlap
overlapcheck <- function(data_input, pair) {
  i <- pair[1]
  j <- pair[2]
  first <- c(data_input$AbsonderungVon[i], data_input$AbsonderungBis[i])
  second <- c(data_input$AbsonderungVon[j], data_input$AbsonderungBis[j])
  if(first %overlaps% second) {
    data_input$AbsonderungVon[i] <- min(c(first, second))
    data_input$AbsonderungBis[i] <- max(c(first, second))
    data_input <- data_input[-c(j),]
  }
  data_input
}


# This function uses the function overlapcheck on the table of one person and gives the result with only correct entries. 
overlapcheck_concise <- function(data_input, testsubject, pair = pairslist) {
  pairslist <- pair
  tdf <- data_input %>% filter(AnonID == testsubject) 
  mylist <- pairslist[[nrow(tdf)]]
  allvalues <- map(mylist, ~overlapcheck(tdf, .x))
  table <- bind_rows(allvalues) 
  table %>% 
    count(rowid) %>% 
    filter(n==length(mylist)) %>% 
    select(-n) %>%  
    left_join(table, by = "rowid") %>% 
    distinct() %>% 
    group_by(rowid) %>% 
    mutate(AbsonderungVon = min(AbsonderungVon)) %>% 
    mutate(AbsonderungBis = max(AbsonderungBis)) %>% 
    distinct()
}


de_duplication <- function(df, methodslist = methodslist) {
  # To save calculation time the complete dataset is split up. I am sure there is an easier way but this is a safe way.
  kps <- df %>% filter(DatensatzKategorie == "Kontakt-COVID-19")
  faelle <- df %>% filter(DatensatzKategorie == "COVID-19")
  einzelne_kps <- kps %>% count(AnonID) %>% filter(n==1) %>% pull(AnonID)
  doppelte_kps <- kps %>% count(AnonID) %>% filter(n>1) %>% pull(AnonID)
  einzelne_faelle <- faelle %>% count(AnonID) %>% filter(n==1) %>% pull(AnonID)
  doppelte_faelle <- faelle %>% count(AnonID) %>% filter(n>1) %>% pull(AnonID)
  einzelne_kps_df <- kps %>% filter(AnonID %in% einzelne_kps)
  doppelte_kps_df <- kps %>% filter(AnonID %in% doppelte_kps)
  einzelne_faelle_df <- faelle %>% filter(AnonID %in% einzelne_faelle)
  doppelte_faelle_df <- faelle %>% filter(AnonID %in% doppelte_faelle)
  
  # Create pairslist
  pairslist <- create_pairslist()
  
  # These functions do the actual work of adjusting the overlapping periods
  doppelte_faelle_df_bereinigt <- bind_rows(map(doppelte_faelle, ~overlapcheck_concise(data_input = doppelte_faelle_df, testsubject = .x, pair = pairslist)))
  doppelte_kps_df_bereinigt <- bind_rows(map(doppelte_kps, ~overlapcheck_concise(data_input = doppelte_kps_df, testsubject = .x, pair = pairslist)))
  
  df <- bind_rows(doppelte_faelle_df_bereinigt, doppelte_kps_df_bereinigt, einzelne_faelle_df, einzelne_kps_df) %>% ungroup()

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Adjust_overlap
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# The following variantes are possible
# 1 
# KP   |----|
#> Case |----|
#> Filter min(kp)>=min(case)
# result should be:  delete(KP)
# 2
# KP      |----|
#> Case |----|
#> Filter min(kp)>=min(case)
# result should be: delete(kp)
# 4
# KP    |--|
#> Case |----|
#> Filter min(kp)>=min(case)
# result should be:  delete(KP) 
# 6
# KP   |--|
#> Case |----|
#> Filter min(kp)>=min(case)
# result should be: delete(KP)
# KP   |----|
#> Case |--|
#> Filter min(kp)>=min(case) 
# result should be: delete(kp)
# 8
# KP    |---|
#> Case |----|
#> Filter min(kp)>=min(case)
# result should be: delete(kp)
# 3
# KP   |----|
#> Case    |----|
#> Filter min(kp)<min(case)
# result should be:  min(KP) - min(Case)
# 5
# KP   |----|
#> Case  |--|
#> Filter min(kp)<min(case)
# result should be: min(KP) - min(case)
# 9
# KP   |----|
#> Case  |---|
#> Filter min(kp)<min(case)
# result should be: min(kp) - min(case)



# This function checks for overlap and adjusts the quarantine if needed
adjustoverlapquarantine <- function(data_input, pair) {
  i <- pair[1]
  j <- pair[2]
  first <- c(data_input$AbsonderungVon[i], data_input$AbsonderungBis[i])
  second <- c(data_input$AbsonderungVon[j], data_input$AbsonderungBis[j])
  firstsequence <- seq(data_input$AbsonderungVon[i], data_input$AbsonderungBis[i], by = 1)
  secondsequence <- seq(data_input$AbsonderungVon[j], data_input$AbsonderungBis[j], by = 1)
  if(first %overlaps% second) {
    if(data_input$DatensatzKategorie[i] == "COVID-19" & data_input$DatensatzKategorie[j] == "Kontakt-COVID-19") {
      if(min(secondsequence)<min(firstsequence)){
        data_input$AbsonderungVon[j] <- min(secondsequence)
        data_input$AbsonderungBis[j] <- min(firstsequence)
        data_input$overlapadjusted[j] <- "adjusted"
      } else {data_input <- data_input[-c(j),]}
      
    }  else if(data_input$DatensatzKategorie[i] == "Kontakt-COVID-19" & data_input$DatensatzKategorie[j] == "COVID-19") {
      if(min(firstsequence)<min(secondsequence)){
        data_input$AbsonderungVon[i] <- min(firstsequence)
        data_input$AbsonderungBis[i] <- min(secondsequence)  
        data_input$overlapadjusted[i] <- "adjusted"
      } else {data_input <- data_input[-c(i),]}
    }
  }
  data_input
}

# This function applies the adjustoverlapquarantine to every testsubject
adjustoverlapquarantine_concise <- function(data_input, testsubject, pairslist = pairslist) {
  tdf <- data_input %>% filter(AnonID == testsubject) 
  mylist <- pairslist[[nrow(tdf)]]
  allvalues <- map(mylist, ~adjustoverlapquarantine(tdf, .x))
  table <- bind_rows(allvalues) 
  changedrowids <- table %>% filter(overlapadjusted == "adjusted") %>% distinct()
  notchangedrowids <- table %>% filter(!rowid %in% changedrowids$rowid) %>% select(rowid) %>% distinct() %>% left_join(tdf, by = "rowid")
  data_output <- bind_rows(changedrowids, notchangedrowids)
  data_output 
}

adjust_overlap <- function(df_deduplicated){
  df <- df_deduplicated
  # Create empty value
  df$overlapadjusted <- NA
  # To save calculation time the complete dataset is split up. I am sure there is an easier way but this is a safe way.
  einzelne_anonIDs <- df %>% count(AnonID) %>% filter(n==1) %>% pull(AnonID)
  doppelte_anonIDs <- df %>% count(AnonID) %>% filter(n>1) %>% pull(AnonID)
  einzelne_anonIDs_df <- df %>% filter(AnonID %in% einzelne_anonIDs)
  doppelte_anonIDs_df <- df %>% filter(AnonID %in% doppelte_anonIDs)
  # Create pairslist
  pairslist <- create_pairslist()
  doppelte_anonIDs_df_bereinigt <- bind_rows(map(doppelte_anonIDs, ~adjustoverlapquarantine_concise(data_input = doppelte_anonIDs_df, 
                                                                                                    testsubject = .x,  
                                                                                                    pairslist = pairslist)))
  # Saving for the publication
  df <- bind_rows(doppelte_anonIDs_df_bereinigt, einzelne_anonIDs_df) %>% ungroup()
  
  df
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Adjoining
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjoincheck <- function(data_input, pair) {
  i <- pair[1] # this is number one
  j <- pair[2] # this is number two
  # what counts as adjoining 
  adjoiningwhentimedifference <- seq(0,7) 
  twoafterone <- data_input$AbsonderungVon[j] - data_input$AbsonderungBis[i]  # if one after two its positive
  oneaftertwo <- data_input$AbsonderungVon[i] - data_input$AbsonderungBis[j] # if two after one its positive
  if(data_input$DatensatzKategorie[i] == "COVID-19" & data_input$DatensatzKategorie[j] == "Kontakt-COVID-19") {
    if(oneaftertwo %in% adjoiningwhentimedifference){
      data_input$adjoiningQandI[i] <- oneaftertwo
      data_input$adjoiningQandI[j] <- oneaftertwo
    } 
  }  else if(data_input$DatensatzKategorie[i] == "Kontakt-COVID-19" & data_input$DatensatzKategorie[j] == "COVID-19") {
    if(twoafterone %in% adjoiningwhentimedifference){
      data_input$adjoiningQandI[i] <- twoafterone
      data_input$adjoiningQandI[j] <- twoafterone
    }  
  }
  data_input
}

adjoincheck_concise <- function(data_input, testsubject, pair = pairslist) {
  
  # Get pairslist
  pairslist <- pair
  
  tdf <- data_input %>% filter(AnonID == testsubject) 
  mylist <- pairslist[[nrow(tdf)]]
  allvalues <- map(mylist, ~adjoincheck(tdf, .x))
  table <- bind_rows(allvalues) 
  changedrowids <- table %>% filter(!is.na(adjoiningQandI)) %>% distinct()
  notchangedrowids <- table %>% filter(!rowid %in% changedrowids$rowid) %>% select(rowid) %>% distinct() %>% left_join(tdf, by = "rowid")
  data_output <- bind_rows(changedrowids, notchangedrowids)
  data_output
}

find_adjoin <- function(df_overlapped) {
  
  # Get the dataframe
  df <- df_overlapped
  
  # Set empty value
  df$adjoiningQandI <- NA
  
  # Split up the df to save computing time
  einzelne_anonIDs <- df %>% count(AnonID) %>% filter(n==1) %>% pull(AnonID)
  doppelte_anonIDs <- df %>% count(AnonID) %>% filter(n>1) %>% pull(AnonID)
  einzelne_anonIDs_df <- df %>% filter(AnonID %in% einzelne_anonIDs)
  doppelte_anonIDs_df <- df %>% filter(AnonID %in% doppelte_anonIDs)
  
  # Create pairslist
  pairslist <- create_pairslist()
  
  # Find the adjoining quarantines and isolations
  doppelte_anonIDs_df_bereinigt <- bind_rows(map(doppelte_anonIDs, ~adjoincheck_concise(data_input = doppelte_anonIDs_df, 
                                                                                        testsubject = .x,
                                                                                        pair = pairslist)))
  
  df <- bind_rows(doppelte_anonIDs_df_bereinigt, einzelne_anonIDs_df)
}


# Final cleaning ----------------------------------------------------------


final_cleaning <- function(df_adjoined, externalinput){
  df_adjoined %>% 
    left_join(externalinput$zeiten, by = c("AbsonderungVon" = "dates")) %>%
    mutate(Meldemonat = paste(year(AbsonderungVon), format.Date(AbsonderungVon, "%m"), sep = "_")) %>% 
    mutate(Meldewoche = paste(year(AbsonderungVon), format.Date(AbsonderungVon, "%W"), sep = "_")) %>% 
    mutate(dauer = as.numeric(AbsonderungBis - AbsonderungVon)) %>% 
    select(-Q_Def_value, -Q_Def_url, -I_Duration_value, -I_Duration_url, -Q_Duration_value, -Q_Duration_url) %>% 
    mutate(result = NA) %>% 
    mutate(result = ifelse(adjoiningQandI == 0, "I_correct_after_Q", result)) %>% 
    mutate(result = ifelse(adjoiningQandI > 0, "I_too_long_after_Q", result)) %>% 
    mutate(result = ifelse(is.na(adjoiningQandI), "No_I_after_Q", result)) %>% 
    mutate(result = ifelse(AbsonderungVon < externalinput$StartDateKP | AbsonderungVon > externalinput$EndDateKP, "outside_of_kp_time", result)) 
}
