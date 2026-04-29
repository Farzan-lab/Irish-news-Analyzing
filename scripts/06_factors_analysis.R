# ============================================================
# 06_factors_analysis.R - Q5: Factors Associated with Engagement (5 marks)
# ============================================================
# Question: Investigate factors associated with higher/lower engagement scores.
# Approach: 9-section comprehensive analysis with multiple perspectives.
# NA Strategy: Complete Case Analysis (remove all NA rows upfront)
# ============================================================

library(dplyr)
library(lubridate)
library(ggplot2)

# ============================================================
# SECTION 1: DATA QUALITY & NA ASSESSMENT
# ============================================================
cat("============================================================\n")
cat("SECTION 1: DATA QUALITY & NA ASSESSMENT\n")
cat("============================================================\n")

total_rows <- nrow(ds)
na_rows <- sum(is.na(ds$engagement_score))
cat("Total rows:", total_rows, "\n")
cat("Rows with NA engagement:", na_rows, "\n")
cat("Percentage NA:", round(na_rows / total_rows * 100, 2), "%\n")

# Check if NA is random (MCAR) or systematic
cat("\n--- NA Distribution by Provider ---\n")
na_by_provider <- ds %>%
  group_by(news_provider) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(engagement_score)),
    na_pct = round(na_count / total * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(na_pct))
print(na_by_provider)

cat("\n--- NA Distribution by Year ---\n")
na_by_year <- ds %>%
  group_by(year) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(engagement_score)),
    na_pct = round(na_count / total * 100, 2),
    .groups = "drop"
  ) %>%
  filter(na_count > 0) %>%
  arrange(desc(na_pct))
print(na_by_year)

# Chi-squared test: Is NA independent of provider?
na_table <- table(ds$news_provider, is.na(ds$engagement_score))
chi_test <- chisq.test(na_table)
cat("\nChi-squared test (NA vs Provider):\n")
cat("X-squared:", chi_test$statistic, "p-value:", chi_test$p.value, "\n")
if (chi_test$p.value < 0.05) {
  cat("Result: NA is NOT randomly distributed (systematic pattern)\n")
} else {
  cat("Result: NA appears randomly distributed (MCAR)\n")
}

# Apply Complete Case Analysis
ds_clean <- ds %>% filter(!is.na(engagement_score))
cat("\nRows after removing NA:", nrow(ds_clean), "\n")

# ============================================================
# SECTION 2: UNIVARIATE ANALYSIS
# ============================================================
cat("\n============================================================\n")
cat("SECTION 2: UNIVARIATE ANALYSIS\n")
cat("============================================================\n")

cat("Mean:", mean(ds_clean$engagement_score), "\n")
cat("Median:", median(ds_clean$engagement_score), "\n")
cat("SD:", sd(ds_clean$engagement_score), "\n")
cat("Min:", min(ds_clean$engagement_score), "\n")
cat("Max:", max(ds_clean$engagement_score), "\n")
cat("Skewness:", round((3 * (mean(ds_clean$engagement_score) - median(ds_clean$engagement_score))) / sd(ds_clean$engagement_score), 4), "\n")

ggplot(ds_clean, aes(x = engagement_score)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(engagement_score)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = median(engagement_score)), color = "green", linetype = "dashed") +
  labs(title = "Distribution of Engagement Score",
       subtitle = "Red = Mean, Green = Median",
       x = "Engagement Score", y = "Count") +
  theme_minimal()

# ============================================================
# SECTION 3: HEADLINE CATEGORY ANALYSIS
# ============================================================
cat("\n============================================================\n")
cat("SECTION 3: HEADLINE CATEGORY ANALYSIS\n")
cat("============================================================\n")

category_stats <- ds_clean %>%
  group_by(headline_category) %>%
  summarise(
    mean_score = mean(engagement_score),
    median_score = median(engagement_score),
    sd_score = sd(engagement_score),
    iqr_score = IQR(engagement_score),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_score))

cat("Top 10 categories by mean engagement:\n")
print(head(category_stats, 10))
cat("\nBottom 10 categories by mean engagement:\n")
print(tail(category_stats, 10))

ggplot(ds_clean, aes(x = reorder(headline_category, engagement_score, FUN = median),
                     y = engagement_score)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  labs(title = "Engagement Score by Headline Category", x = "Category", y = "Score") +
  theme_minimal()

# ANOVA + Effect Size
anova_category <- aov(engagement_score ~ headline_category, data = ds_clean)
cat("\n--- ANOVA: Category Effect ---\n")
summary(anova_category)

ss <- summary(anova_category)[[1]]
eta_sq_cat <- ss$`Sum Sq`[1] / sum(ss$`Sum Sq`)
cat("Eta-squared:", round(eta_sq_cat, 4), "- Category explains", round(eta_sq_cat * 100, 2), "% of variance\n")

# Post-hoc: Top 5 vs Bottom 5
top5_cat <- head(category_stats$headline_category, 5)
bottom5_cat <- tail(category_stats$headline_category, 5)
ds_top_bottom <- ds_clean %>%
  filter(headline_category %in% c(top5_cat, bottom5_cat)) %>%
  mutate(group = ifelse(headline_category %in% top5_cat, "Top 5", "Bottom 5"))

t_test_cat <- t.test(engagement_score ~ group, data = ds_top_bottom)
cat("\nT-test Top 5 vs Bottom 5 categories: p-value =", t_test_cat$p.value, "\n")

# Main category analysis
ds_clean$main_category <- sapply(strsplit(ds_clean$headline_category, "\\."), function(x) x[1])

main_cat_stats <- ds_clean %>%
  group_by(main_category) %>%
  summarise(
    mean_score = mean(engagement_score),
    median_score = median(engagement_score),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_score))

cat("\n--- Main Category Summary ---\n")
print(main_cat_stats)

ggplot(ds_clean, aes(x = reorder(main_category, engagement_score, FUN = median),
                     y = engagement_score, fill = main_category)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Engagement Score by Main Category", x = "Category", y = "Score") +
  theme_minimal() + theme(legend.position = "none")

# ============================================================
# SECTION 4: NEWS PROVIDER ANALYSIS
# ============================================================
cat("\n============================================================\n")
cat("SECTION 4: NEWS PROVIDER ANALYSIS\n")
cat("============================================================\n")

provider_stats <- ds_clean %>%
  group_by(news_provider) %>%
  summarise(
    mean_score = mean(engagement_score),
    median_score = median(engagement_score),
    sd_score = sd(engagement_score),
    iqr_score = IQR(engagement_score),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_score))

print(provider_stats)

ggplot(ds_clean, aes(x = reorder(news_provider, engagement_score, FUN = median),
                     y = engagement_score, fill = news_provider)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Engagement Score by News Provider", x = "Provider", y = "Score") +
  theme_minimal() + theme(legend.position = "none")

# ANOVA + Effect Size + Tukey HSD
anova_provider <- aov(engagement_score ~ news_provider, data = ds_clean)
cat("\n--- ANOVA: Provider Effect ---\n")
summary(anova_provider)

ss_prov <- summary(anova_provider)[[1]]
eta_sq_prov <- ss_prov$`Sum Sq`[1] / sum(ss_prov$`Sum Sq`)
cat("Eta-squared:", round(eta_sq_prov, 4), "- Provider explains", round(eta_sq_prov * 100, 2), "% of variance\n")

cat("\n--- Tukey HSD: Significant Pairwise Differences ---\n")
tukey_result <- TukeyHSD(anova_provider)
sig_pairs <- as.data.frame(tukey_result$news_provider) %>% filter(`p adj` < 0.05)
print(sig_pairs)

# ============================================================
# SECTION 5: TEMPORAL TRENDS
# ============================================================
cat("\n============================================================\n")
cat("SECTION 5: TEMPORAL TREND ANALYSIS\n")
cat("============================================================\n")

yearly_trend <- ds_clean %>%
  group_by(year) %>%
  summarise(mean_score = mean(engagement_score), .groups = "drop")

ggplot(yearly_trend, aes(x = year, y = mean_score)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "red") +
  labs(title = "Yearly Trend of Engagement Score", x = "Year", y = "Mean Score") +
  theme_minimal()

trend_model <- lm(mean_score ~ year, data = yearly_trend)
cat("Slope:", coef(trend_model)[2], "\n")
cat("R-squared:", summary(trend_model)$r.squared, "\n")
cat("P-value:", summary(trend_model)$coefficients[2, 4], "\n")

# Per-provider trends
yearly_provider <- ds_clean %>%
  group_by(news_provider, year) %>%
  summarise(mean_score = mean(engagement_score), .groups = "drop")

ggplot(yearly_provider, aes(x = year, y = mean_score, color = news_provider)) +
  geom_line(linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Yearly Engagement Trend by Provider", x = "Year", y = "Mean Score", color = "Provider") +
  theme_minimal()

# Monthly seasonality
ds_clean$month_name <- month(ds_clean$date_parsed, label = TRUE)
monthly_pattern <- ds_clean %>%
  group_by(month_name) %>%
  summarise(mean_score = mean(engagement_score), .groups = "drop")

ggplot(monthly_pattern, aes(x = month_name, y = mean_score, group = 1)) +
  geom_line(linewidth = 1, color = "steelblue") + geom_point(size = 2, color = "red") +
  labs(title = "Monthly Seasonality of Engagement Score", x = "Month", y = "Mean Score") +
  theme_minimal()

# Day of week pattern
ds_clean$day_of_week <- wday(ds_clean$date_parsed, label = TRUE)
daily_pattern <- ds_clean %>%
  group_by(day_of_week) %>%
  summarise(mean_score = mean(engagement_score), .groups = "drop")

ggplot(daily_pattern, aes(x = day_of_week, y = mean_score, group = 1)) +
  geom_line(linewidth = 1, color = "steelblue") + geom_point(size = 2, color = "red") +
  labs(title = "Day of Week Pattern in Engagement Score", x = "Day", y = "Mean Score") +
  theme_minimal()

anova_day <- aov(engagement_score ~ day_of_week, data = ds_clean)
cat("\n--- ANOVA: Day of Week Effect ---\n")
summary(anova_day)

# ============================================================
# SECTION 6: HEADLINE LENGTH ANALYSIS
# ============================================================
cat("\n============================================================\n")
cat("SECTION 6: HEADLINE LENGTH ANALYSIS\n")
cat("============================================================\n")

ds_clean$headline_length <- nchar(ds_clean$headline_text)
ds_clean$word_count <- sapply(strsplit(ds_clean$headline_text, "\\s+"), length)

cor_length <- cor.test(ds_clean$headline_length, ds_clean$engagement_score)
cat("Character Length - Correlation:", cor_length$estimate, "P-value:", cor_length$p.value, "\n")

cor_words <- cor.test(ds_clean$word_count, ds_clean$engagement_score)
cat("Word Count - Correlation:", cor_words$estimate, "P-value:", cor_words$p.value, "\n")

ggplot(ds_clean, aes(x = headline_length, y = engagement_score)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", color = "red") +
  geom_smooth(method = "loess", color = "blue", linetype = "dashed") +
  labs(title = "Headline Length vs Engagement", subtitle = "Red = Linear, Blue = LOESS",
       x = "Length (characters)", y = "Score") +
  theme_minimal()

# Binned analysis
ds_clean$length_bin <- cut(ds_clean$headline_length,
                           breaks = c(0, 30, 50, 70, 90, 120, Inf),
                           labels = c("0-30", "31-50", "51-70", "71-90", "91-120", "120+"))

binned_stats <- ds_clean %>%
  group_by(length_bin) %>%
  summarise(mean_score = mean(engagement_score), count = n(), .groups = "drop")

ggplot(binned_stats, aes(x = length_bin, y = mean_score, fill = length_bin)) +
  geom_col() + geom_text(aes(label = paste0("n=", count)), vjust = -0.5, size = 3) +
  labs(title = "Mean Engagement by Headline Length Range", x = "Length Range", y = "Mean Score") +
  theme_minimal() + theme(legend.position = "none")

# ============================================================
# SECTION 7: INTERACTION EFFECTS
# ============================================================
cat("\n============================================================\n")
cat("SECTION 7: INTERACTION EFFECTS\n")
cat("============================================================\n")

anova_interaction <- aov(engagement_score ~ main_category * news_provider, data = ds_clean)
cat("--- Two-way ANOVA: Category x Provider ---\n")
summary(anova_interaction)

interaction_data <- ds_clean %>%
  group_by(main_category, news_provider) %>%
  summarise(mean_score = mean(engagement_score), .groups = "drop")

ggplot(interaction_data, aes(x = main_category, y = mean_score,
                             color = news_provider, group = news_provider)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2) +
  labs(title = "Interaction: Category x Provider",
       subtitle = "Non-parallel lines suggest interaction effects",
       x = "Main Category", y = "Mean Score", color = "Provider") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================
# SECTION 8: MULTIPLE REGRESSION
# ============================================================
cat("\n============================================================\n")
cat("SECTION 8: MULTIPLE REGRESSION\n")
cat("============================================================\n")

model_full <- lm(engagement_score ~ main_category + news_provider + year +
                   headline_length + month + day_of_week,
                 data = ds_clean %>% mutate(month = month(date_parsed)))

cat("--- Full Regression Model ---\n")
summary(model_full)

# Compare individual R-squared values
cat("\n--- R-squared Comparison ---\n")
cat("Category alone:", round(summary(lm(engagement_score ~ main_category, data = ds_clean))$r.squared, 4), "\n")
cat("Provider alone:", round(summary(lm(engagement_score ~ news_provider, data = ds_clean))$r.squared, 4), "\n")
cat("Year alone:", round(summary(lm(engagement_score ~ year, data = ds_clean))$r.squared, 4), "\n")
cat("Length alone:", round(summary(lm(engagement_score ~ headline_length, data = ds_clean))$r.squared, 4), "\n")
cat("Full model:", round(summary(model_full)$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(summary(model_full)$adj.r.squared, 4), "\n")

# ============================================================
# SECTION 9: KEY FINDINGS SUMMARY
# ============================================================
cat("\n============================================================\n")
cat("SECTION 9: KEY FINDINGS SUMMARY\n")
cat("============================================================\n")

cat("1. CATEGORY: Eta-sq =", round(eta_sq_cat, 4), "- explains", round(eta_sq_cat * 100, 2), "% of variance\n")
cat("2. PROVIDER: Eta-sq =", round(eta_sq_prov, 4), "- explains", round(eta_sq_prov * 100, 2), "% of variance\n")
cat("3. YEARLY TREND: Slope =", round(coef(trend_model)[2], 4),
    "-", ifelse(coef(trend_model)[2] > 0, "increasing", "decreasing"), "over time\n")
cat("4. HEADLINE LENGTH: r =", round(cor_length$estimate, 4),
    "-", ifelse(abs(cor_length$estimate) < 0.1, "negligible", 
         ifelse(abs(cor_length$estimate) < 0.3, "weak", "moderate")), "relationship\n")
cat("5. FULL MODEL: R-sq =", round(summary(model_full)$r.squared, 4),
    "- all factors explain", round(summary(model_full)$r.squared * 100, 2), "% of variance\n")
cat("6. NA: Removed", na_rows, "rows (", round(na_rows/total_rows*100, 2), "%) -",
    ifelse(chi_test$p.value < 0.05, "NA is systematic", "NA appears random"), "\n")
