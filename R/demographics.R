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
    legend.background = element_rect(fill = NA),
    plot.title = element_text(size=12)
  ) +
  scale_fill_manual(
    values = c("#84329B","#FFAD00", "#3c3c3b")
  )

age_plt

ggsave("output/demographics/age-hc-2526.png", plot = age_plt,
       width = 6, height = 4)

# Sex

sex_plt <- hc_processed %>%
  filter(
    sex %in% c("Male", "Female")
    ) %>%
  ggplot(aes(x = sex, fill = sex)) +
  geom_bar(color = "black") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 20e3), expand = c(0,0)) +
  labs(
    fill = "",
    x = "",
    y = "",
    title = "Sex of Birmingham NHS Health Check Attendees (25/26)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=12)
  ) +
  scale_fill_manual(
    values = c("#84329B","#FFAD00")
  )
sex_plt
ggsave("output/demographics/sex-hc-2526.png", plot = sex_plt,
       width = 6, height = 4)

# Ethnicity

eth_plt <- ggplot(hc_processed, aes(x = broad_ethnicity, fill = broad_ethnicity)) +
  geom_bar(color = "black") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 12e3), expand = c(0,0)) +
  labs(
    fill = "",
    x = "",
    y = "",
    title = "Broad Ethnicity of Birmingham NHS Health Check Attendees (25/26)"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(size=12)
  ) +
  scale_fill_manual(
    values = c("#84329B","#D00070", "#DC582A", "#FFAD00", "#00A9E0", "#75BC22")
  )
eth_plt
ggsave("output/demographics/ethnicity-hc-2526.png", plot = eth_plt,
       width = 6, height = 4)