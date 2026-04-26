# ============================================================
# 05_max_engagement.R - Q4: Max Engagement Year & Largest Increase
# ============================================================
# Question: In which year did TheJournal.ie record max engagement?
#           Which provider shows the largest increase over the years?
# ============================================================

# --- Part A: Year of Maximum Engagement for TheJournal.ie ---
cat("=== Part A: TheJournal.ie Maximum Engagement ===\n")

# Filter data for TheJournal.ie
journal_data <- ds %>%
  filter(news_provider == "TheJournal.ie")

# Find the year with maximum engagement score
max_year <- journal_data$year[which.max(journal_data$engagement_score)]
max_score <- max(journal_data$engagement_score, na.rm = TRUE)

cat("Year with maximum engagement score:", max_year, "\n")
cat("Maximum engagement score:", max_score, "\n")

# --- Part B: Provider with Largest Increase Over Years ---
cat("\n=== Part B: Provider with Largest Increase ===\n")

# Calculate yearly average engagement score for each provider
yearly_avg <- ds %>%
  group_by(news_provider, year) %>%
  summarise(mean_score = mean(engagement_score, na.rm = TRUE), .groups = "drop")

# Fit linear model for each provider and extract the slope
# Positive slope = increasing trend, higher slope = larger increase
slopes <- yearly_avg %>%
  group_by(news_provider) %>%
  summarise(slope = coef(lm(mean_score ~ year))[2], .groups = "drop") %>%
  arrange(desc(slope))

# Display all providers sorted by slope
cat("All providers sorted by engagement increase (slope):\n")
print(slopes)

# The first row is the provider with the largest increase
cat("\nProvider with largest increase:\n")
print(slopes[1, ])
