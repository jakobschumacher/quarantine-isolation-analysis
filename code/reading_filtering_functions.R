tidy_demographiedata <- function(data_demographie_file){
  data_demographie_file %>% 
    read_delim(delim = ";", show_col_types = FALSE) %>% 
    filter(BEZ == "12")  %>% 
    summarise('0 to 6' = sum(E_E00_01 + E_E01_02  + E_E02_03  + E_E03_05  + E_E05_06  +E_E06_07),
              '7 to 17' = sum(E_E07_08 + E_E08_10  + E_E10_12 + E_E12_14 + E_E14_15  + E_E15_18),
              '18 to 64' = sum(E_E18U25 + E_E25U55 + E_E55U65),
              '65 to 110' = sum(E_E65U80 + E_E80U110)) %>% 
    pivot_longer(cols = everything())
}

read_survnetdata <- function(datafile = tar_read(data_survnet_file)){
  datafile %>% 
    read_delim(delim = ",", show_col_types = FALSE)  %>%  # reading in the date
    rowid_to_column() %>% # Add RowID (this is needed for the purr::map function)
    rename(AbsonderungBis = AbsonderunBis) %>% # Correct SurvNet mistake
    mutate(Meldedatum = as.Date(Meldedatum, format = "%d.%m.%Y")) %>% 
    mutate(AbsonderungVon = as.Date(AbsonderungVon, format = "%d.%m.%Y")) %>% 
    mutate(AbsonderungBis = as.Date(AbsonderungBis, format = "%d.%m.%Y")) %>% 
    mutate(AgeGroup = recode_factor(AgeGroup, "<7" = "0 to 6", "7bis17" = "7 to 17", "18bis64" = "18 to 64", ">64" = "65 to 110", .ordered = TRUE))
}

set_filter <- function(dataset, externalinput){  
  dataset %>% 
    mutate(emptydates = "noemptydates") %>% 
    mutate(emptydates = ifelse(is.na(AbsonderungVon), "emptydates", emptydates)) %>%
    mutate(emptydates = ifelse(is.na(AbsonderungBis), "emptydates", emptydates)) %>%
    mutate(wrongID = "korrektID") %>% 
    mutate(wrongID = ifelse(AnonID == externalinput$wrong_id, "wrongID", wrongID)) %>% 
    mutate(wrongID = ifelse(AnonID == externalinput$wrong_id2, "wrongID", wrongID)) %>% 
    mutate(outofrange = "inrange") %>% 
    mutate(outofrange = ifelse(AbsonderungVon < externalinput$StartDate | AbsonderungVon > (externalinput$EndDate + 14), "outofrange", outofrange)) %>%
    mutate(outofrange = ifelse(AbsonderungBis < externalinput$StartDate | AbsonderungBis > (externalinput$EndDate + 14), "outofrange", outofrange)) %>%
    mutate(outofrange = ifelse(Meldedatum < externalinput$StartDate | Meldedatum > (externalinput$EndDate + 14), "outofrange", outofrange)) %>% 
    mutate(typingerror = "notypingerror") %>% 
    mutate(abstandVonBis = AbsonderungBis - AbsonderungVon) %>% # create time periods used for filtering
    mutate(abstandMeldedatumVon = Meldedatum - AbsonderungVon) %>% # create time periods used for filtering
    mutate(typingerror = ifelse(abstandVonBis > 30, "typingerror", typingerror)) %>%
    mutate(typingerror = ifelse(abstandVonBis < 1, "typingerror", typingerror)) %>%
    mutate(typingerror = ifelse(abstandMeldedatumVon < -30, "typingerror", typingerror)) %>%
    mutate(typingerror = ifelse(abstandMeldedatumVon > 30, "typingerror", typingerror)) %>% 
    mutate(filtervar = ifelse(emptydates == "noemptydates" &
                                wrongID == "korrektID" &
                                outofrange == "inrange" &
                                typingerror == "notypingerror", 
                              "keep", "dontkeep" )) 
}


filtering_the_dataset <- function(dataset){
  dataset %>%  
    filter(filtervar == "keep") %>% 
    select(-filtervar, -emptydates, -wrongID, -outofrange, -typingerror, -abstandMeldedatumVon, -abstandVonBis)
}
