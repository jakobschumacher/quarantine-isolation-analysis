library(targets)

tar_option_set(workspace_on_error = TRUE)

source("code/external_input.R")
source("code/reading_filtering_functions.R")
source("code/transforming_functions.R")
source("code/calculating_results.R")
source("code/graphs_tables.R")

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
  tar_target(df_prefiltered, set_filter(df_raw, externalinput)),
  tar_target(df_filtered, filtering_the_dataset(df_prefiltered)),
  # Deduplication
  tar_target(df_deduplicated, de_duplication(df_filtered)),
  # Adjust overlap
  tar_target(df_overlapped, adjust_overlap(df_deduplicated)),
  # Find adjoining quarantines and isolations
  tar_target(df_adjoined, find_adjoin(df_overlapped)),
  # Final cleaning
  tar_target(df, final_cleaning(df_adjoined, externalinput)),
  # numerical results
  tar_target(results, get_numerical_results(df, demographiedaten, externalinput)),
  # graphs
  tar_target(plot_duration, create_figure_duration(df, demographiedaten), format = "file"),
  tar_target(plot_adjoining, create_figure_adjoining(df), format = "file"),
  tar_target(plot_epicurve, create_figure_epicurve(df, demographiedaten, results, externalinput), format = "file"),
  tar_target(plot_inclusionexclusion, create_figure_inclusionexclusion(results), format = "file"),
  tar_target(plot_measures_table, create_measures_table(results), format = "file")

)
