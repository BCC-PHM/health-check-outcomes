library(readxl)
library(dplyr)
library(ggplot2)
source("R/config.R")

# data file path
data_path <- file.path(
  path_prefix,
  "processed-hc-data-2526.xlsx"
)

hc_processed <- read_excel(
  data_path
)

# Age

age_plt <- ggplot(hc_processed, aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 5, na.rm=TRUE, color = "black") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 10e3), expand = c(0,0)) +
  labs(
    fill = "",
    x = "Age (Years)",
    y = "",
    title = "Age of Birmingham NHS Health Check Attendees (25/26)"
  ) +
  theme(
    legend.position.inside = TRUE,
    legend.position = c(0.85,0.85),
    legend.background = element_rect(fill = NA)
  ) +
  scale_fill_manual(
    values = c("#84329B","#FFAD00", "#3c3c3b")
  )

age_plt

ggsave("output/demographics/age-hc-2526.png", width = 6, height = 4)