set_filter <- function(dataset, externalinput){  

  dataset %>% 
    mutate(definitionvar = "notdefined") %>% 
    mutate(definitionvar = ifelse(!is.na(AbsonderungVon) & !is.na(AbsonderungBis), "defined", definitionvar)) %>% 
    mutate(definitionvar = ifelse(!is.na(AbsonderungVon) & !is.na(AbsonderungBis), "defined", definitionvar)) %>% 
    mutate(definitionvar = ifelse(AnonID == externalinput$wrong_id, "wrongID", definitionvar)) %>% 
    mutate(definitionvar = ifelse(AnonID == externalinput$wrong_id2, "wrongID", definitionvar)) %>% 
    mutate(outofrange = "inrange") %>% 
    mutate(outofrange = ifelse(AbsonderungVon < externalinput$StartDate | AbsonderungVon > (externalinput$EndDate + 14), "outofrange", outofrange)) %>%
    mutate(outofrange = ifelse(AbsonderungBis < externalinput$StartDate | AbsonderungBis > (externalinput$EndDate + 14), "outofrange", outofrange)) %>%
    mutate(outofrange = ifelse(Meldedatum < externalinput$StartDate | Meldedatum > (externalinput$EndDate + 14), "outofrange", outofrange)) %>% 
    mutate(filtervar = "korrekt") %>% 
    mutate(filtervar = ifelse(abstandVonBis > 30, "typingerror", filtervar)) %>%
    mutate(filtervar = ifelse(abstandVonBis < 1, "typingerror", filtervar)) %>%
    mutate(filtervar = ifelse(abstandMeldedatumVon < -30, "typingerror", filtervar)) %>%
    mutate(filtervar = ifelse(abstandMeldedatumVon > 30, "typingerror", filtervar)) %>% 
    select(rowid, definitionvar, outofrange, filtervar)
}


get_info_about_filtering <- function(methodslist = methodslist, rows_to_be_filtered = rows_to_be_filtered){
  
  methodslist$queried <- nrow(rows_to_be_filtered)  
  
  methodslist$definitionvar <- rows_to_be_filtered %>% 
    select(definitionvar) %>% 
    table()
  
  
  methodslist$outofrange <- rows_to_be_filtered %>% 
    filter(definitionvar == "defined") %>% 
    select(outofrange) %>% 
    table()
  
  methodslist$wrongEntries <- rows_to_be_filtered %>% 
    filter(definitionvar == "defined") %>% 
    filter(outofrange == "inrange") %>% 
    select(filtervar) %>% 
    table()  
 
  methodslist 
  
}


filtering_the_dataset <- function(dataset, rows_to_be_filtered){
  rows_to_be_filtered <- 
    rows_to_be_filtered %>% 
    select(rowid, filtervar)
  
  dataset <- dataset %>%  
    left_join(rows_to_be_filtered) %>% 
    filter(filtervar == "korrekt") %>% 
    select(-filtervar)
  
  dataset
}
