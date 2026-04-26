# ============================================================
# 06_factors_analysis.R - Q5: Factors Associated with Engagement
# ============================================================
# Question: Investigate factors associated with higher/lower engagement scores.
# Factors: Category, Provider, Time/Year, Headline Length
# ============================================================

# Remove rows with NA engagement scores
ds_clean <- ds %>% filter(!is.na(engagement_score))

# ---- Factor 1: Headline Category ----
cat("=== Factor 1: Engagement Score by Headline Category ===\n")

# Summary statistics per category
category_stats <- ds_clean %>%
  group_by(headline_category) %>%
  summarise(
    mean_score = mean(engagement_score),
    median_score = median(engagement_score),
    sd_score = sd(engagement_score),
    count = n()
  ) %>%
  arrange(desc(mean_score))

print(category_stats)

# Boxplot: Distribution of engagement score across categories
ggplot(ds_clean, aes(x = reorder(headline_category, engagement_score, FUN = median),
                     y = engagement_score)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Engagement Score by Headline Category",
       x = "Category", y = "Engagement Score")

# ---- Factor 2: News Provider ----
cat("\n=== Factor 2: Engagement Score by News Provider ===\n")

# Summary statistics per provider
provider_stats <- ds_clean %>%
  group_by(news_provider) %>%
  summarise(
    mean_score = mean(engagement_score),
    median_score = median(engagement_score),
    sd_score = sd(engagement_score),
    count = n()
  ) %>%
  arrange(desc(mean_score))

print(provider_stats)

# Boxplot: Distribution of engagement score across providers
ggplot(ds_clean, aes(x = reorder(news_provider, engagement_score, FUN = median),
                     y = engagement_score)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Engagement Score by News Provider",
       x = "Provider", y = "Engagement Score")

# ---- Factor 3: Time Trend ----
cat("\n=== Factor 3: Yearly Trend ===\n")

# Calculate yearly average engagement score
yearly_trend <- ds_clean %>%
  group_by(year) %>%
  summarise(mean_score = mean(engagement_score))

print(yearly_trend)

# Line plot: Engagement score trend over years
ggplot(yearly_trend, aes(x = year, y = mean_score)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly Trend of Engagement Score",
       x = "Year", y = "Mean Engagement Score")

# ---- Factor 4: Headline Length ----
cat("\n=== Factor 4: Headline Length ===\n")

# Create headline length column
ds_clean$headline_length <- nchar(ds_clean$headline_text)

# Pearson correlation test
cor_result <- cor.test(ds_clean$headline_length, ds_clean$engagement_score)
cat("Correlation:", cor_result$estimate, "\n")
cat("P-value:", cor_result$p.value, "\n")

# Scatter plot with regression line
ggplot(ds_clean, aes(x = headline_length, y = engagement_score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(title = "Headline Length vs Engagement Score",
       x = "Headline Length (characters)", y = "Engagement Score")

# ---- Statistical Tests ----
cat("\n=== ANOVA: Engagement by Category ===\n")
anova_category <- aov(engagement_score ~ headline_category, data = ds_clean)
summary(anova_category)

cat("\n=== ANOVA: Engagement by Provider ===\n")
anova_provider <- aov(engagement_score ~ news_provider, data = ds_clean)
summary(anova_provider)

# ---- Multiple Regression ----
cat("\n=== Multiple Regression Model ===\n")
model <- lm(engagement_score ~ headline_category + news_provider + year + headline_length,
            data = ds_clean)
summary(model)
