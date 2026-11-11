# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(janitor)

# Read the CSV file
raw <- read_csv("imd_daily_rainfall.csv",
                na = c("", "NA", "ND"),
                show_col_types = FALSE) |>
  clean_names()

# Check required columns
needed <- c("state", "date", "actual", "normal", "category")
missing_cols <- setdiff(needed, names(raw))

if (length(missing_cols) > 0) {
  stop("Missing expected columns: ",
       paste(missing_cols, collapse = ", "))
}

# Data cleaning and transformation
rain <- raw |>
  transmute(
    state = str_squish(str_to_upper(state)),
    date = dmy(date),
    actual_rainfall = as.numeric(actual),
    normal_rainfall = as.numeric(normal),
    daily_category = factor(category,
                            levels = c("LE", "E", "N", "D", "LD", "NR"))
  ) |>
  filter(!is.na(date), !is.na(actual_rainfall),
         !is.na(normal_rainfall)) |>
  mutate(
    departure_pct = if_else(normal_rainfall > 0,
                            (actual_rainfall - normal_rainfall) /
                              normal_rainfall * 100,
                            NA_real_),
    
    rainfall_status = if_else(
      actual_rainfall < normal_rainfall,
      "Below Normal",
      "At/Above Normal"
    )
  )

# Calculate daily average rainfall
daily_avg <- rain |>
  filter(!str_detect(state, "COUNTRY")) |>
  group_by(date) |>
  summarise(
    avg_daily_actual = mean(actual_rainfall),
    avg_daily_normal = mean(normal_rainfall),
    .groups = "drop"
  ) |>
  arrange(date)

# Calculate state-wise average rainfall
state_avg <- rain |>
  filter(!str_detect(state, "COUNTRY")) |>
  group_by(state) |>
  summarise(
    avg_daily_actual = mean(actual_rainfall),
    avg_daily_normal = mean(normal_rainfall),
    .groups = "drop"
  ) |>
  arrange(desc(avg_daily_actual))

# Export cleaned data to CSV
write_csv(rain, "cleaned_rainfall_tableau.csv", na = "")

# Display dataset information
cat("Rows:", nrow(rain), "\n")
cat("Columns:", ncol(rain), "\n")
cat("Date range:", format(min(rain$date)), "to",
    format(max(rain$date)), "\n")