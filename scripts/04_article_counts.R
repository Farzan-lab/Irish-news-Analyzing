# ============================================================
# 04_article_counts.R - Q3: Article Counts by Category & Provider (3 marks)
# ============================================================
# Question: Compute total articles for each category and provider.
#           Use a SINGLE R function to display Min, Max, Mean per provider.
# Depth: Beyond basic tapply — explore distribution and coverage patterns.
# ============================================================
library(dplyr)
library(ggplot2)
library(scales)

# --- Step 1: Count total articles for each combination of provider and category ---
article_counts <- as.data.frame(table(ds$news_provider, ds$headline_category))
colnames(article_counts) <- c("news_provider", "headline_category", "total_articles")

# --- Step 2: Required SINGLE function — tapply with summary ---
cat("=== Statistical Summary per News Provider (Single Function) ===\n")
tapply(article_counts$total_articles, article_counts$news_provider, summary)

# --- Step 3: Additional Depth — Category Coverage Analysis ---

# How many categories does each provider actually cover?
cat("\n=== Category Coverage per Provider ===\n")
coverage <- article_counts %>%
  filter(total_articles > 0) %>%
  group_by(news_provider) %>%
  summarise(
    categories_covered = n(),
    total_articles = sum(total_articles),
    mean_per_category = round(mean(total_articles), 2),
    max_category_count = max(total_articles),
    .groups = "drop"
  )
print(coverage)

# Which categories are covered by ALL providers vs only some?
cat("\n=== Category Coverage Breadth ===\n")
category_coverage <- article_counts %>%
  filter(total_articles > 0) %>%
  group_by(headline_category) %>%
  summarise(
    covered_by_n_providers = n(),
    total_across_providers = sum(total_articles),
    .groups = "drop"
  ) %>%
  arrange(desc(covered_by_n_providers))

all_providers_count <- length(unique(article_counts$news_provider))

cat("Categories covered by ALL providers:\n")
print(category_coverage %>% filter(covered_by_n_providers == all_providers_count))

cat("\nCategories covered by only 1 provider:\n")
print(category_coverage %>% filter(covered_by_n_providers == 1))

# Visualize the distribution (fixed y-axis formatting)
ggplot(article_counts %>% filter(total_articles > 0),
       aes(x = news_provider, y = total_articles)) +
  geom_boxplot(fill = "lightblue") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Article Counts Across Categories per Provider",
       subtitle = "Each data point represents a category's article count for that provider",
       x = "News Provider", y = "Total Articles per Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))