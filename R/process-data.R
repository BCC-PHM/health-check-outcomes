library(readxl)
library(dplyr)
library(janitor)
source("R/config.R")

# data file path
hc_data_path <- file.path(
  path_prefix,
  "TPP AND EMIS FULL REFERRAL 25 26.xlsx"
)

# Load all sheets and combine
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
  
# process data
hc_processed <- hc_unprocessed %>% 
  # Join ethnicity groups
  left_join(
    read_excel("data/lookups.xlsx", sheet = "ethnicity") %>%
      clean_names(),
    by = join_by("ethnicity_record")
  ) %>%
  # Join smoking status groups
  left_join(
    read_excel("data/lookups.xlsx", sheet = "smoking") %>%
      clean_names(),
    by = join_by("smoking_status_code" == "smoking_status")
  ) %>%
  mutate(
    # Convert age to numeric
    age = as.numeric(age),
    
    # Extract numeric component of BMI
    bmi_value = as.numeric(
      stringr::str_extract(bmi_value, "\\d+(?:\\.\\d+)?")
      ),
    # Remove improbable values from BMI
    bmi_value = case_when(
      # Remove BMIs > 80 (n = 66)
      bmi_value > 80 ~ NA,
      # Remove BMIs < 10 (n = 24)
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
    ),
    
    # Weight Management Service:
    #  - Eligibility: BMI > 23 if (Black and Asian) BMI > 25 if Otherwise
    #  - Breakdown: accepted, declined, not offered, not eligible
    
    weight_management_service_eligible = case_when(
      broad_ethnicity %in% c("Black", "Asian") & bmi_value > 23 ~ TRUE,
      bmi_value > 25 ~ TRUE,
      TRUE ~ FALSE
    ),
    weight_management_service = case_when(
      !is.na(referred_to_weight_man_declined_code) ~ "Declined",
      grepl("Refer", refered_to_weight_man_prog_code) ~ "Accepted",
      weight_management_service_eligible ~ "Not offered",
      TRUE ~ "Not eligible"
    ),
    
    # Smoking advice given:
    #   
    #   Eligibility: smoker (any level)
    #   Smoking cessation service (Stop smoking or local service)
    #   Eligibility: smoker (any level)
    #   Breakdown: accepted, declined, not offered
    
    smoking_advice = case_when(
      grepl("declined", smoking_cess_advice_code) ~ "Declined",
      !is.na(smoking_cess_advice_code) ~ "Accepted",
      smoker == "Yes" ~ "Not offered",
      smoker == "No" ~ "Not eligible",
      smoker == "Unknown" ~ "Unknown eligibility",
      TRUE ~ "Unexpected smoking status"
    ),
    
    # stop smoking service
    #   
    #   Eligibility: smoker (any level)
    #   Smoking cessation service (Stop smoking or local service)
    #   Eligibility: smoker (any level)
    #   Breakdown: accepted, declined, not offered
    
    smoking_service = case_when(
      grepl("declined", ref_stop_smoking_service_code) ~ "Declined",
      !is.na(ref_stop_smoking_service_code) ~ "Accepted",
      smoker == "Yes" ~ "Not offered",
      smoker == "No" ~ "Not eligible",
      smoker == "Unknown" ~ "Unknown eligibility",
      TRUE ~ "Unexpected smoking status"
    ),
    
    # Alcohol advise given
    # 
    #   For whole population (no data for eligibility)
    
    alcohol_advice = case_when(
      !is.na(lifestyle_advice_regarding_alcohol_code) ~ "Accepted",
      TRUE ~ "Unknown eligibility"
    ),
    
    # Lifestyle Services
    # 
    #   For whole population (no data for eligibility)
    
    lifestyle_services = case_when(
      !is.na(referral_to_lifestyle_services_code) ~ "Accepted",
      TRUE ~ "Unknown eligibility"
    ),
    
    # Exercise program referral:
    #   
    #  For whole population (no data for eligibility)
    exercise_program = case_when(
      !is.na(ref_exercise_prog_code) ~ "Accepted",
      TRUE ~ "Unknown eligibility"
    )
  ) %>%
  # Reduce data frame to necessary columns
  select(
    # GP info
    gp_name, gp_code, 
    # Patient demographics
    sex, age, ethnic_group, broad_ethnicity, 
    # Recorded values
    bmi_value, h_ba1c_value,
    # Health check outcomes
    weight_management_service, smoking_advice, smoking_service,
    alcohol_advice, lifestyle_services, exercise_program
  )

# Save processed data
writexl::write_xlsx(
  hc_processed, 
  file.path(path_prefix, "processed-hc-data-2526.xlsx")
  )