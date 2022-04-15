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

###################################################
# De duplication
###################################################
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
  
  # Saving for the publication
  
  # Putting the table back together
  output_list <- list(doppelte_faelle_df_bereinigt, doppelte_kps_df_bereinigt, einzelne_faelle_df, einzelne_kps_df) 
  output_list
}


