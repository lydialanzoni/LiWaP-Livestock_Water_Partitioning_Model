# Library
library(data.table)
source(file = "R/core_LiWaP.R")
source(file = "R/run_LiWaP.R")

# Dataframe
water_input_df <- as.data.table(fread("your_directory/water_input.csv"))
water_output_df <- as.data.table(fread("Inputs/parameters_LWB.csv"))

# Run Module
water_results <- run_liwap(
  water_input_df = water_input_df,
  water_output_df = water_output_df
)

# Write results
fwrite(water_results, "your_directory/water_results.csv")
