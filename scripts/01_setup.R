# ============================================================
# 01_setup.R - Library Loading and Data Preparation
# ============================================================

# Load required libraries

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Load dataset - UPDATE THIS PATH to your actual file location
ds <- read.csv("C:/Users/farza/Uni/S2/Fundamental of Data Science/assignment2/ireland_news.csv", stringsAsFactors = FALSE)

# Parse publish_date: remove ordinal suffixes (st, nd, rd, th) and convert to Date
ds$date_parsed <- dmy(gsub("(\\d+)(st|nd|rd|th)", "\\1", ds$publish_date))

# Extract year and month from parsed date
ds$year <- year(ds$date_parsed)
ds$month <- month(ds$date_parsed)

# Remove rows with NA dates
ds <- ds %>% filter(!is.na(date_parsed))

# Create a clean dataset without NA engagement scores (used in multiple questions)
ds_clean <- ds %>% filter(!is.na(engagement_score))

# Display basic dataset info
cat("============================================================\n")
cat("DATASET SUMMARY\n")
cat("============================================================\n")
cat("Total rows:", nrow(ds), "\n")
cat("Total columns:", ncol(ds), "\n")
cat("Date range:", as.character(min(ds$date_parsed)), "to", as.character(max(ds$date_parsed)), "\n")
cat("News providers:", length(unique(ds$news_provider)), "\n")
cat("Headline categories:", length(unique(ds$headline_category)), "\n")
cat("NA engagement scores:", sum(is.na(ds$engagement_score)), "\n")
cat("NA percentage:", round(sum(is.na(ds$engagement_score)) / nrow(ds) * 100, 2), "%\n")
