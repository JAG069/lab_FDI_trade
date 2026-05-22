#################################
#### Lab FDI and Trade Data
#### May 22, 2026 
#### Jimmy Guzman



# clear environment
rm(list = ls())

 
# new packages we need for Census and BEA

library(censusapi)
library(bea.R)
library(fredr)
library(tidyverse)


# Top 10 U.S. export destinations by year

exports_cty_yr <- getCensus(
  name = "timeseries/intltrade/exports/naics",
  vars = c("ALL_VAL_YR", "YEAR", "CTY_CODE", "CTY_NAME"),
  time = "from 2000",
  MONTH = "12",
  show_call = TRUE)

head(exports_cty_yr)

# Clean regional/aggregate codes
exports_cty_yr_clean <- exports_cty_yr %>%
  filter(!(substr(CTY_CODE, 1, 1) == "0" |
             substr(CTY_CODE, 2, 2) == "X" |
             substr(CTY_CODE, 1, 1) == "-"))

# Convert values to numeric and billions USD
exports_cty_yr_clean <- exports_cty_yr_clean %>%
  mutate(
    ALL_VAL_YR = as.numeric(ALL_VAL_YR) / 1000000000,
    YEAR = as.numeric(YEAR))

# Check for NAs
sum(is.na(exports_cty_yr_clean$ALL_VAL_YR))

# Create top 10 list by year
top10_exports <- exports_cty_yr_clean %>%
  group_by(YEAR) %>%
  slice_max(order_by = ALL_VAL_YR, n = 10, with_ties = FALSE) %>%
  arrange(YEAR, desc(ALL_VAL_YR))

# Add rankings
top10_exports <- top10_exports %>%
  group_by(YEAR) %>%
  arrange(-ALL_VAL_YR, CTY_NAME) %>%
  mutate(rank = row_number()) %>%
  ungroup()

## GRAPH FOR 2015 ----

yrplot <- 2015

ggplot(top10_exports %>% filter(YEAR == yrplot),
       aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = ALL_VAL_YR / 2,
                width = ALL_VAL_YR,
                height = .5,
                color = CTY_NAME,
                fill = CTY_NAME),
            show.legend = FALSE) +
  geom_text(aes(x = ALL_VAL_YR,
                y = rank,
                label = CTY_NAME),
            nudge_x = 15,
            show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10,
                  minor_breaks = NULL) +
  labs(
    x = "Export Value (billions USD)",
    y = "Ranking by Exports",
    title = paste("Top 10 Destinations for U.S. Exports:", yrplot)) +
  theme_minimal()

ggsave("top10_exports_2015.png", width = 8, height = 6)

## GRAPH FOR 2025 ----

yrplot <- 2025

ggplot(top10_exports %>% filter(YEAR == yrplot),
       aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = ALL_VAL_YR / 2,
                width = ALL_VAL_YR,
                height = .5,
                color = CTY_NAME,
                fill = CTY_NAME),
            show.legend = FALSE) +
  geom_text(aes(x = ALL_VAL_YR,
                y = rank,
                label = CTY_NAME),
            nudge_x = 15,
            show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10,
                  minor_breaks = NULL) +
  labs(
    x = "Export Value (billions USD)",
    y = "Ranking by Exports",
    title = paste("Top 10 Destinations for U.S. Exports:", yrplot)) +
  theme_minimal()

ggsave("top10_exports_2025.png", width = 8, height = 6)
