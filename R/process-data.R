library(readxl)
library(dplyr)

source("R/config.R")

hc_data_path <- file.path(
  path_prefix,
  "TPP AND EMIS FULL REFERRAL 25 26.xlsx"
)

sheets <- excel_sheets(hc_data_path)
hc_data <- data.table::rbindlist(
  lapply(
    sheets,
    read_excel,
    path = hc_data_path
  ),
  use.names=TRUE
)
