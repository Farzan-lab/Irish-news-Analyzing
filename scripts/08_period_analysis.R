# ============================================================
# 08_period_analysis.R - Q7: Period-Based Analysis 2019-2020 (3 marks)
# ============================================================
# Question: Filter 2019-2020, assign quarterly periods (1-8),
#           compute articles by period & category, generate boxplot.
# ============================================================

library(dplyr)
library(lubridate)
library(ggplot2)

# Filter data for 2019 and 2020 only
ds_filtered <- ds %>% filter(year(date_parsed) %in% c(2019, 2020))

# Create Period column based on quarterly date ranges
ds_filtered <- ds_filtered %>%
  mutate(Period = case_when(
    date_parsed >= as.Date("2019-01-01") & date_parsed <= as.Date("2019-03-31") ~ "Period 1",
    date_parsed >= as.Date("2019-04-01") & date_parsed <= as.Date("2019-06-30") ~ "Period 2",
    date_parsed >= as.Date("2019-07-01") & date_parsed <= as.Date("2019-09-30") ~ "Period 3",
    date_parsed >= as.Date("2019-10-01") & date_parsed <= as.Date("2019-12-31") ~ "Period 4",
    date_parsed >= as.Date("2020-01-01") & date_parsed <= as.Date("2020-03-31") ~ "Period 5",
    date_parsed >= as.Date("2020-04-01") & date_parsed <= as.Date("2020-06-30") ~ "Period 6",
    date_parsed >= as.Date("2020-07-01") & date_parsed <= as.Date("2020-09-30") ~ "Period 7",
    date_parsed >= as.Date("2020-10-01") & date_parsed <= as.Date("2020-12-31") ~ "Period 8"
  ))

# Compute total number of articles by Period and headline_category
period_counts <- ds_filtered %>%
  group_by(Period, headline_category) %>%
  summarise(total_articles = n(), .groups = "drop")

cat("=== Article Counts by Period (sample) ===\n")
print(head(period_counts, 20))

# Generate boxplot
ggplot(period_counts, aes(x = Period, y = total_articles)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of Total Articles by Period (2019-2020)",
       x = "Period", y = "Total Number of Articles") +
  theme_minimal()
