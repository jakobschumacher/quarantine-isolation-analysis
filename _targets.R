library(targets)

# tar_option_set(debug = "df_deduplicated")

source("code/external_input.R")
source("code/reading_functions.R")
source("code/filtering_functions.R")
source("code/recoding_functions.R")



tar_target(
  churn_cor, {
    cor <- compute_cor(churn_recipe)
    plot <- plot_cor(cor)
    write_csv(cor, "cor.csv")
    ggsave(plot = plot, filename = "cor.png", width = 8, height = 8)
    # The return value must be a vector of paths to the files we write:
    paste0("cor.", c("csv", "png"))
  },
  format = "file" # Tells targets to track the return value (path) as a file.
)


# End this file with a list of target objects.
list(
  # Reading in data
  tar_target(externalinput, read_externalinput()),
  tar_target(demographiedata, read_demographiedata()),
  tar_target(results, {
    write_csv(tibble("varname" = character(), "value" = double()), "data/results.csv")
    "data/results.csv"}, 
    format = "file"),
  tar_target(methodslist, methodslist <- list()),
  tar_target(df_raw, read_survnetdata()),
  # Finding what to filter
  tar_target(rows_to_be_filtered, set_filter(df_raw, externalinput)),
  tar_target(store_info_about_filtering, get_info_about_filtering(methodslist, rows_to_be_filtered)),
  tar_target(df_filtered, filtering_the_dataset(df_raw, rows_to_be_filtered)),
  tar_target(list_deduplicated, de_duplication(df_filtered))
)
