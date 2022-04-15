library(targets)

tar_option_set(workspace_on_error = TRUE)
# tar_option_set(debug = "df_deduplicated")

source("code/external_input.R")
source("code/reading_functions.R")
source("code/filtering_functions.R")
source("code/transforming_functions.R")


# End this file with a list of target objects.
list(
  # Reading in data
  tar_target(data_demographie_file, "data/EWR202012E_Matrix.csv", format = "file"),
  tar_target(data_survnet_file, "data/raw_data_survnet.csv", format = "file"),
  tar_target(data_oxcgrt_file, "data/OxCGRT_latest.csv", format = "file"),
  tar_target(externalinput, read_externalinput()),
  # Initial cleaning
  tar_target(df_raw, read_survnetdata(data_survnet_file)),
  tar_target(demographiedaten, tidy_demographiedata(data_demographie_file)),
  # Filtering
  tar_target(rows_to_be_filtered, set_filter(df_raw, externalinput)),
  tar_target(store_info_about_filtering, get_info_about_filtering(rows_to_be_filtered), format = "file"),
  tar_target(df_filtered, filtering_the_dataset(df_raw, rows_to_be_filtered)),
  # Deduplication
  tar_target(list_deduplicated, de_duplication(df_filtered)),
  tar_target(store_info_about_deduplication, get_info_about_deduplication(list_deduplicated), format = "file"),
  tar_target(df_deduplicated, list_to_df_after_deduplication(list_deduplicated)),
  # Adjust overlap
  tar_target(list_overlapadjusted, adjust_overlap(df_deduplicated)),
  tar_target(store_info_about_overlapadjusted, get_info_about_overlapadjust(list_overlapadjusted), format = "file"),
  tar_target(df_overlapped, list_to_df_after_overlapadjust(list_overlapadjusted)),
  # Find adjoining quarantines and isolations
  tar_target(df_adjoined, find_adjoin(df_overlapped)),
  # Final cleaning
  tar_target(df, final_cleaning(df_adjoined, externalinput))

)
