library(readxl)
library(dplyr)
library(janitor)
source("R/config.R")

hc_data_path <- file.path(
  path_prefix,
  "TPP AND EMIS FULL REFERRAL 25 26.xlsx"
)

sheets <- excel_sheets(hc_data_path)
hc_unprocessed <- data.table::rbindlist(
  lapply(
    sheets,
    read_excel,
    path = hc_data_path
  ),
  use.names=TRUE
) %>% 
  clean_names() %>%
  select(-contains("date")) 
  

hc_processed <- hc_unprocessed %>% 
  left_join(
    read_excel("data/lookups.xlsx", sheet = "ethnicity") %>%
      clean_names(),
    by = join_by("ethnicity_record" == "ethnicity_raw")
  ) %>%
  left_join(
    read_excel("data/lookups.xlsx", sheet = "broad_ethnicity") %>%
      clean_names(),
    by = join_by("ethnic_group")
  ) %>%
  mutate(
    broad_ethnicity = case_when(
      is.na(broad_ethnicity) ~ "Not linked",
      TRUE ~ broad_ethnicity
    ),
    # Extract numeric component of BMI
    bmi_value = as.numeric(
      stringr::str_extract(bmi_value, "\\d+(?:\\.\\d+)?")
      ),
    # Remove silly values from BMI
    bmi_value = case_when(
      # Remove BMI's > 80 (n = 66)
      bmi_value > 80 ~ NA,
      # Remove BMI's < 10 (n = 24)
      bmi_value < 10 ~ NA,
      # Otherwise keep
      TRUE ~ bmi_value
    ),
    h_ba1c_value = as.numeric(
      stringr::str_extract(h_ba1c_value, "\\d+(?:\\.\\d+)?")
    ),
    # Remove improbable values
    h_ba1c_value = case_when(
      # Remove values less than 10 mmol/mol (n = 218)
      h_ba1c_value < 10 ~ NA,
      # Otherwise keep
      TRUE ~ h_ba1c_value
    )
  )

## 


# eth_lookup <- hc_unprocessed %>%
#   count(ethnicity_record) %>%
#   arrange(desc(n)) %>%
#   left_join(
#     read_excel("data/lookups.xlsx", sheet = "ethnicity") %>%
#       clean_names() %>%
#       left_join(
#         read_excel("data/lookups.xlsx", sheet = "broad_ethnicity") %>%
#           clean_names(),
#         by = join_by("ethnic_group")),
#     by = join_by("ethnicity_record" == "ethnicity_raw")
#     )
# 
# writexl::write_xlsx(eth_lookup, "ethnicity_lookup.xlsx")
