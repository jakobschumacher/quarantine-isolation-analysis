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

read_survnetdata <- function(datafile = data_survnet_file){
  datafile %>% 
    read_delim(delim = ",", show_col_types = FALSE)  %>%  # reading in the date
    rowid_to_column() %>% # Add RowID (this is needed for the purr::map function)
    rename(AbsonderungBis = AbsonderunBis) %>% # Correct SurvNet mistake
    mutate(Meldedatum = as.Date(Meldedatum, format = "%d.%m.%Y")) %>% 
    mutate(AbsonderungVon = as.Date(AbsonderungVon, format = "%d.%m.%Y")) %>% 
    mutate(AbsonderungBis = as.Date(AbsonderungBis, format = "%d.%m.%Y")) %>% 
    mutate(Meldemonat = paste(year(AbsonderungVon), format.Date(AbsonderungVon, "%m"), sep = "_")) %>% 
    mutate(Meldewoche = paste(year(AbsonderungVon), format.Date(AbsonderungVon, "%W"), sep = "_")) %>% 
    mutate(dauer = as.numeric(AbsonderungBis - AbsonderungVon)) %>% 
    mutate(abstandVonBis = AbsonderungBis - AbsonderungVon) %>% # create time periods used for filtering
    mutate(abstandMeldedatumVon = Meldedatum - AbsonderungVon) %>% # create time periods used for filtering
    mutate(AgeGroup = recode_factor(AgeGroup, "<7" = "0 to 6", "7bis17" = "7 to 17", "18bis64" = "18 to 64", ">64" = "65 to 110", .ordered = TRUE))
}