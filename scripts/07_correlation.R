# ============================================================
# 07_correlation.R - Q6: Correlation & Trend Analysis (4 marks)
# ============================================================
# Q6A: Top 3 categories with strongest correlation between
#      yearly article volume and yearly mean engagement per provider.
# Q6B: Top 3 categories with most significant increasing trends.
# NA Strategy: na.rm + complete.obs (two-stage handling)
# ============================================================

library(dplyr)

# ===== Part A: Correlation Analysis =====
cat("=== Q6A: Top 3 Categories by Correlation ===\n")

# Stage 1: na.rm = TRUE for yearly means
# Stage 2: use = "complete.obs" for correlation
yearly_data <- ds %>%
  group_by(news_provider, headline_category, year) %>%
  summarise(
    yearly_count = n(),
    yearly_mean_score = mean(engagement_score, na.rm = TRUE),
    n_valid = sum(!is.na(engagement_score)),
    n_na = sum(is.na(engagement_score)),
    .groups = "drop"
  ) %>%
  filter(!is.nan(yearly_mean_score))

# Warn about high NA combinations
high_na <- yearly_data %>% filter(n_na / (n_valid + n_na) > 0.5)
if (nrow(high_na) > 0) {
  cat("Warning:", nrow(high_na), "combinations have >50% NA\n")
}

# Compute correlation (need at least 3 data points)
cor_results <- yearly_data %>%
  group_by(news_provider, headline_category) %>%
  filter(n() >= 3) %>%
  summarise(
    correlation = cor(yearly_count, yearly_mean_score, use = "complete.obs"),
    p_value = cor.test(yearly_count, yearly_mean_score)$p.value,
    .groups = "drop"
  ) %>%
  filter(!is.na(correlation))

# Top 3 per provider by absolute correlation strength
top3_cor <- cor_results %>%
  group_by(news_provider) %>%
  arrange(desc(abs(correlation))) %>%
  slice_head(n = 3)

print(top3_cor)

# ===== Part B: Trend Analysis =====
cat("\n=== Q6B: Top 3 Categories with Increasing Trends ===\n")

yearly_counts <- ds %>%
  group_by(news_provider, headline_category, year) %>%
  summarise(yearly_count = n(), .groups = "drop")

trend_results <- yearly_counts %>%
  group_by(news_provider, headline_category) %>%
  filter(n() >= 3) %>%
  summarise(
    slope = coef(lm(yearly_count ~ year))[2],
    p_value = summary(lm(yearly_count ~ year))$coefficients[2, 4],
    .groups = "drop"
  )

# Filter positive slopes, sort by significance
top3_trend <- trend_results %>%
  filter(slope > 0) %>%
  group_by(news_provider) %>%
  arrange(p_value) %>%
  slice_head(n = 3)

print(top3_trend)
