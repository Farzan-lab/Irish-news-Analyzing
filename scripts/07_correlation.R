# ============================================================
# 07_correlation.R - Q6: Correlation & Trend Analysis
# ============================================================
# Q6A: Top 3 categories with strongest correlation between
#      yearly article volume and yearly mean engagement per provider.
# Q6B: Top 3 categories with most significant increasing trends
#      in yearly article counts per provider.
# ============================================================

# ===== Part A: Correlation Analysis =====
cat("=== Q6A: Top 3 Categories by Correlation ===\n")

# Calculate yearly article count and mean engagement for each provider-category-year
yearly_data <- ds %>%
  group_by(news_provider, headline_category, year) %>%
  summarise(
    yearly_count = n(),
    yearly_mean_score = mean(engagement_score, na.rm = TRUE),
    .groups = "drop"
  )

# Compute Pearson correlation for each provider-category pair
# Requires at least 3 years of data for meaningful correlation
cor_results <- yearly_data %>%
  group_by(news_provider, headline_category) %>%
  filter(n() >= 3) %>%
  summarise(
    correlation = cor(yearly_count, yearly_mean_score, use = "complete.obs"),
    p_value = cor.test(yearly_count, yearly_mean_score)$p.value,
    .groups = "drop"
  )

# Select top 3 by absolute correlation strength per provider
top3_cor <- cor_results %>%
  group_by(news_provider) %>%
  arrange(desc(abs(correlation))) %>%
  slice_head(n = 3)

print(top3_cor)

# ===== Part B: Trend Analysis =====
cat("\n=== Q6B: Top 3 Categories with Increasing Trends ===\n")

# Calculate yearly article counts per provider-category
yearly_counts <- ds %>%
  group_by(news_provider, headline_category, year) %>%
  summarise(yearly_count = n(), .groups = "drop")

# Fit linear regression for each combination to get slope and p-value
trend_results <- yearly_counts %>%
  group_by(news_provider, headline_category) %>%
  filter(n() >= 3) %>%
  summarise(
    slope = coef(lm(yearly_count ~ year))[2],
    p_value = summary(lm(yearly_count ~ year))$coefficients[2, 4],
    .groups = "drop"
  )

# Filter for positive slopes (increasing) and select top 3 most significant
top3_trend <- trend_results %>%
  filter(slope > 0) %>%
  group_by(news_provider) %>%
  arrange(p_value) %>%
  slice_head(n = 3)

print(top3_trend)
