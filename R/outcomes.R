library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(writexl)
source("R/config.R")

gp_lookup <- read_excel(
  "data/External Megamap April 2026.xlsx",
  skip = 5
) %>%
  clean_names() %>%
  rename(gp_code = code, locality = constituency_locality) %>%
  select(
    gp_code, pcn, locality
  ) %>%
    filter(
      !is.na(gp_code)
    )

# data file path
data_path <- file.path(
  path_prefix,
  "processed-hc-data-2526.xlsx"
)

hc_processed <- read_excel(
  data_path
) %>% 
  filter(
    gp_name != "Organisation name"
  ) %>%
  left_join(
    gp_lookup, 
    by = join_by("gp_code")
  )

hc_long <- hc_processed %>%
  pivot_longer(
    cols = c("weight_management_service", "smoking_advice", "smoking_service",
             "alcohol_advice", "lifestyle_services", "exercise_program"),
    names_to = "service",
    values_to = "outcome"
  ) %>%
  mutate(
    gp_name = stringr::str_to_title(gp_name),
    service =stringr::str_to_title(
      gsub("_", " ", service)
    ),
    outcome = factor(
      outcome, 
      levels = c(
        "Unknown eligibility","Not eligible", 
        "Not offered","Declined","Accepted"
        )
      )
    )

### Overall ###

overall_plt <- hc_long %>%
  count(service, outcome) %>%
  ggplot(aes(y = service, x = n, fill = outcome)) +
  geom_col(position = "fill", color = "black") + 
  theme_bw() +
  labs(
    y = "NHS Health Check Service",
    x = "Percentage of Attendees (2025/26)",
    fill = ""
  ) +
  theme(
    legend.position = "top",
    legend.justification = c(1,0)
  ) +
  scale_x_continuous(
    expand = c(0,0),
    labels = scales::percent
  ) +
  scale_fill_manual(
    values = c("#3c3c3b", "#FFAD00", "#DC582A", "#D00070", "#84329B")
  ) +
  guides(fill = guide_legend(reverse=TRUE))

overall_plt

ggsave("output/outcomes/hc-outcomes-general-2526.png", plot = overall_plt,
       width = 6, height = 4)

# Save overall data
overall_outcomes_data <- hc_long %>%
  count(service, outcome) %>%
  pivot_wider(
    names_from = outcome,
    values_from = n
  ) %>%
  replace(., is.na(.), 0) %>%
  mutate(
    `Total Attendees` = rowSums(across(where(is.numeric)))
  ) %>%
  relocate(`Total Attendees`, .after = service) 

write_xlsx(
  overall_outcomes_data, 
  "output/data/overall-hc-outcomes-2526.xlsx"
  )

# Sex

sex_plt <- hc_long %>%
  filter(
    # Removes two entries
    sex %in% c("Male", "Female")
  ) %>%
  count(service, outcome, sex) %>%
  ggplot(aes(y = sex, x = n, fill = outcome)) +
  geom_col(position = "fill", color = "black") + 
  theme_bw() +
  labs(
    y = "",
    x = "Percentage of Attendees (2025/26)",
    fill = ""
  ) +
  theme(
    legend.position = "top",
    legend.justification = c(1,0),
    strip.background = element_rect(fill="white"),
    panel.spacing.x = unit(1.5, "lines"),
  ) +
  scale_x_continuous(
    expand = c(0,0),
    labels = scales::percent
  ) +
  scale_fill_manual(
    values = c("#3c3c3b", "#FFAD00", "#DC582A", "#D00070", "#84329B")
  ) +
  guides(fill = guide_legend(reverse=TRUE)) +
  facet_wrap(~factor(service,
                     levels = unique(hc_long$service)),
             ncol = 2)

sex_plt

ggsave("output/outcomes/hc-outcomes-by-sex-2526.png", plot = sex_plt,
       width = 6, height = 5)

# Save sex data

sex_outcomes_data <- hc_long %>%
  count(service, outcome, sex) %>%
  filter(sex %in% c("Male", "Female")) %>%
  pivot_wider(
    names_from = outcome,
    values_from = n
  ) %>%
  replace(., is.na(.), 0) %>%
  mutate(
    `Total Attendees` = rowSums(across(where(is.numeric)))
  ) %>%
  relocate(`Total Attendees`, .after = service) %>%
  group_by(service) %>%
  group_split()

sex_outcomes_list <- as.list(sex_outcomes_data)
names(sex_outcomes_list) <- sort(unique(hc_long$service))

write_xlsx(
  sex_outcomes_list, 
  "output/data/sex-hc-outcomes-2526.xlsx"
)

### Broad Ethnicity ###

eth_plt <- hc_long %>%
  count(service, outcome, broad_ethnicity) %>%
  ggplot(aes(y = broad_ethnicity, x = n, fill = outcome)) +
  geom_col(position = "fill", color = "black") + 
  theme_bw() +
  labs(
    y = "",
    x = "Percentage of Attendees (2025/26)",
    fill = ""
  ) +
  theme(
    legend.position = "top",
    legend.justification = c(1,0),
    strip.background = element_rect(fill="white"),
    panel.spacing.x = unit(1.5, "lines"),
  ) +
  scale_x_continuous(
    expand = c(0,0),
    labels = scales::percent
  ) +
  scale_fill_manual(
    values = c("#3c3c3b", "#FFAD00", "#DC582A", "#D00070", "#84329B")
  ) +
  guides(fill = guide_legend(reverse=TRUE)) +
  facet_wrap(~factor(service,
                     levels = unique(hc_long$service)),
             ncol = 2)

eth_plt

ggsave("output/outcomes/hc-outcomes-by-ethnicity-2526.png", plot = eth_plt,
       width = 6, height = 8)

# Save ethnicity data

eth_outcomes_data <- hc_long %>%
  count(service, outcome, broad_ethnicity) %>%
  pivot_wider(
    names_from = outcome,
    values_from = n
  ) %>%
  replace(., is.na(.), 0) %>%
  mutate(
    `Total Attendees` = rowSums(across(where(is.numeric)))
  ) %>%
  relocate(`Total Attendees`, .after = service) %>%
  group_by(service) %>%
  group_split()

eth_outcomes_list <- as.list(eth_outcomes_data)
names(eth_outcomes_list) <- sort(unique(hc_long$service))

write_xlsx(
  eth_outcomes_list, 
  "output/data/ethnicity-hc-outcomes-2526.xlsx"
)


### Locality ###

local_plt <- hc_long %>%
  filter(locality != "Solihull") %>%
  count(service, outcome, locality) %>%
  ggplot(aes(y = locality, x = n, fill = outcome)) +
  geom_col(position = "fill", color = "black") + 
  theme_bw() +
  labs(
    y = "",
    x = "Percentage of Attendees (2025/26)",
    fill = ""
  ) +
  theme(
    legend.position = "top",
    legend.justification = c(1,0),
    strip.background = element_rect(fill="white"),
    panel.spacing.x = unit(1.5, "lines"),
  ) +
  scale_x_continuous(
    expand = c(0,0),
    labels = scales::percent
  ) +
  scale_fill_manual(
    values = c("#3c3c3b", "#FFAD00", "#DC582A", "#D00070", "#84329B")
  ) +
  guides(fill = guide_legend(reverse=TRUE)) +
  facet_wrap(~factor(service,
                     levels = unique(hc_long$service)),
             ncol = 2)

local_plt

ggsave("output/outcomes/hc-outcomes-by-locality-2526.png", plot = local_plt,
       width = 6, height = 8)

# Save ethnicity data
local_outcomes_data <- hc_long %>%
  count(service, outcome, locality) %>%
  pivot_wider(
    names_from = outcome,
    values_from = n
  ) %>%
  replace(., is.na(.), 0) %>%
  mutate(
    `Total Attendees` = rowSums(across(where(is.numeric)))
  ) %>%
  relocate(`Total Attendees`, .after = service) %>%
  group_by(service) %>%
  group_split()

local_outcomes_list <- as.list(local_outcomes_data)
names(local_outcomes_list) <- sort(unique(hc_long$service))

write_xlsx(
  local_outcomes_list, 
  "output/data/locality-hc-outcomes-2526.xlsx"
)

### GP ###

service_acronym = list(
  "Weight Management Service" = "WMS",
  "Smoking Advice" = "SA",
  "Smoking Service" = "SS",
  "Alcohol Advice" = "AA",
  "Lifestyle Services" = "LS",
  "Exercise Program" = "EP"
)

for (service_i in unique(hc_long$service)) {
  data_i <- hc_long %>%
    filter(service == service_i,
           locality != "Solihull") %>%
    count(service, outcome, gp_code, locality) 
  
  accepted_or_declined <- data_i %>%
    filter(
      outcome %in% c("Accepted", "Declined")
    ) %>%
    group_by(gp_code) %>%
    summarise(
      offered = sum(n)
    )
  
  offered_perc <- data_i %>%
    group_by(gp_code) %>%
    summarise(
      attendees = sum(n)
    ) %>%
    left_join(
      accepted_or_declined,
      by = join_by(gp_code)
    ) %>%
    mutate(
      offered = ifelse(is.na(offered), 0, offered),
      offer_frac = offered / attendees
    ) %>%
    arrange(offer_frac)
  
  data_i$gp_code = factor(
    data_i$gp_code,
    levels = offered_perc$gp_code
  )
  
  gp_plt_i <- ggplot(
    data_i,
    aes(y = gp_code, x = n, fill = outcome)
    ) +
    geom_col(position = "fill", color = "black") + 
    theme_bw() +
    labs(
      y = "",
      x = "Percentage of Attendees (2025/26)",
      fill = "",
      title = paste(service_i, "Outcomes by GP (2025/26)")
    ) +
    theme(
      legend.position = "top",
      legend.justification = c(1,0),
      strip.background = element_rect(fill="white"),
      panel.spacing.x = unit(1.5, "lines"),
    ) +
    scale_x_continuous(
      expand = c(0,0),
      labels = scales::percent
    ) +
    scale_fill_manual(
      values = c("#3c3c3b", "#FFAD00", "#DC582A", "#D00070", "#84329B")
    ) +
    guides(fill = guide_legend(reverse=TRUE)) +
    facet_wrap(~locality,
               ncol = 2,
               scale = "free_y")
  
  save_name <- paste0(
    "output/outcomes/hc-outcomes-by-GP-",
    service_acronym[service_i],
    ".png"
  )
  
  ggsave(save_name, plot = gp_plt_i,
         width = 8, height = 14)
}