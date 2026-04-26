# ============================================================
# 04_article_counts.R - Q3: Article Counts by Category & Provider
# ============================================================
# Question: Compute total articles for each category and provider.
#           Use a SINGLE R function to display Min, Max, Mean per provider.
# ============================================================

# Step 1: Count total articles for each combination of provider and category
# (Multiple functions allowed for data preparation)
article_counts <- as.data.frame(table(ds$news_provider, ds$headline_category))
colnames(article_counts) <- c("news_provider", "headline_category", "total_articles")

# Step 2: Display Min, Max, Mean using a SINGLE function (tapply with summary)
# tapply applies summary() to total_articles, grouped by news_provider
cat("=== Statistical Summary per News Provider ===\n")
tapply(article_counts$total_articles, article_counts$news_provider, summary)
