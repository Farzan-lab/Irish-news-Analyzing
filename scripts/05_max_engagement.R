# ============================================================
# 05_max_engagement.R - Q4: Max Engagement Year & Largest Increase
# ============================================================
# Goal: Find max engagement year for TheJournal.ie and trend analysis
# Strategy: Use ggplot2 for visual trend representation
# ============================================================
library(ggplot2)
library(dplyr)

# Preliminary Cleaning: Filter out negative engagement scores
ds <- ds %>% filter(is.na(engagement_score) | engagement_score >= 0)

# --- Part A: Year of Maximum Engagement for TheJournal.ie ---
cat("=== Part A: TheJournal.ie Maximum Engagement ===\n")

# Filter for TheJournal.ie and remove NAs for precise maximum calculation
journal_data <- ds %>%
  filter(news_provider == "TheJournal.ie", !is.na(engagement_score))

# Calculate and display max metrics
max_year <- journal_data$year[which.max(journal_data$engagement_score)]
max_score <- max(journal_data$engagement_score)

cat("Year with maximum engagement:", max_year, "\n")
cat("Maximum engagement score:", max_score, "\n")

# --- Part B: Provider with Largest Increase Over Years ---
cat("\n=== Part B: Provider with Largest Increase ===\n")

# Aggregate data by provider and year, calculating mean scores
yearly_avg <- ds %>%
  group_by(news_provider, year) %>%
  summarise(
    mean_score = mean(engagement_score, na.rm = TRUE),
    n_total = n(),
    n_na = sum(is.na(engagement_score)),
    .groups = "drop"
  ) %>%
  filter(!is.nan(mean_score)) # Remove groups with no valid engagement data

# Use a linear model (slope) to determine the rate of increase
slopes <- yearly_avg %>%
  group_by(news_provider) %>%
  summarise(slope = coef(lm(mean_score ~ year))[2], .groups = "drop") %>%
  arrange(desc(slope))

cat("Top provider by engagement increase (Slope):\n")
print(slopes[1, ])

# --- Part C: Visualization of Engagement Trends ---
cat("\n=== Part C: Generating Line Plot ===\n")

# Create the line chart using ggplot2
engagement_plot <- ggplot(yearly_avg, aes(x = year, y = mean_score, color = news_provider, group = news_provider)) +
  geom_line(linewidth = 1.2) +         # Draw trends lines
  geom_point(size = 2.5) +            # Add points for each year
  theme_minimal() +                   # Professional clean theme
  labs(
    title = "News Providers: Engagement Score Trends",
    subtitle = "Comparing average yearly engagement across providers",
    x = "Year",
    y = "Average Engagement Score",
    color = "News Provider"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  ) +
  # Ensure x-axis only shows integer years
  scale_x_continuous(breaks = min(yearly_avg$year):max(yearly_avg$year))

# Execute the plot rendering
print(engagement_plot)