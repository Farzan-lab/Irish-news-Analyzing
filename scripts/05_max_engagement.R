# ============================================================
# 05_max_engagement.R - Q4: Max Engagement Year & Largest Increase (5 marks)
# ============================================================
# Question: In which year did TheJournal.ie record max engagement?
#           Which provider shows the largest increase over the years?
# NA Strategy: Removal for max (Part A), na.rm for means (Part B)
# ============================================================

# --- Part A: Year of Maximum Engagement for TheJournal.ie ---
cat("=== Part A: TheJournal.ie Maximum Engagement ===\n")

# Filter for TheJournal.ie AND remove NA engagement scores
# Strategy: Removal — replacing NA with mean/median could create a false maximum
journal_data <- ds %>%
  filter(news_provider == "TheJournal.ie", !is.na(engagement_score))

# Report NA removal
journal_all <- ds %>% filter(news_provider == "TheJournal.ie")
na_count <- sum(is.na(journal_all$engagement_score))
cat("TheJournal.ie total rows:", nrow(journal_all), "\n")
cat("Rows with NA engagement removed:", na_count, "\n")
cat("Rows used for analysis:", nrow(journal_data), "\n")

# Find the year with maximum engagement score
max_year <- journal_data$year[which.max(journal_data$engagement_score)]
max_score <- max(journal_data$engagement_score)
cat("Year with maximum engagement:", max_year, "\n")
cat("Maximum engagement score:", max_score, "\n")

# --- Part B: Provider with Largest Increase Over Years ---
cat("\n=== Part B: Provider with Largest Increase ===\n")
cat("Total NA in engagement_score:", sum(is.na(ds$engagement_score)), "\n")

# Strategy: na.rm = TRUE when computing yearly means
# This keeps all years but ignores NA values within each year
yearly_avg <- ds %>%
  group_by(news_provider, year) %>%
  summarise(
    mean_score = mean(engagement_score, na.rm = TRUE),
    n_total = n(),
    n_valid = sum(!is.na(engagement_score)),
    n_na = sum(is.na(engagement_score)),
    .groups = "drop"
  ) %>%
  filter(!is.nan(mean_score))

# Warn about years with high NA percentage
high_na <- yearly_avg %>% filter(n_na / n_total > 0.5)
if (nrow(high_na) > 0) {
  cat("\nWarning: Provider-year combinations with >50% NA:\n")
  print(high_na)
}

# Fit linear model for each provider and extract the slope
slopes <- yearly_avg %>%
  group_by(news_provider) %>%
  summarise(slope = coef(lm(mean_score ~ year))[2], .groups = "drop") %>%
  arrange(desc(slope))

cat("\nAll providers sorted by engagement increase (slope):\n")
print(slopes)
cat("\nProvider with largest increase:\n")
print(slopes[1, ])
