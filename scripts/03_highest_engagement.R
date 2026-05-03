# ============================================================
# 03_highest_engagement.R - Q2: Highest Mean Engagement & Lowest Articles (4 marks)
# ============================================================
# Question: Which provider has the highest mean engagement?
#           Which headline category has the lowest number of articles?
# ============================================================

# Remove negative engagement scores before analysis
ds <- ds %>% filter(is.na(engagement_score) | engagement_score >= 0)

# --- Part A: Provider with Highest Mean Engagement ---
cat("=== Part A: Mean Engagement by Provider ===\n")

# Check NA count before analysis
cat("NA values in engagement_score:", sum(is.na(ds$engagement_score)), "\n")
cat("Strategy: Remove NA rows (imputation would bias per-provider means)\n\n")

# Calculate mean engagement score for each provider (excluding NA values)
provider_mean <- ds %>%
  filter(!is.na(engagement_score)) %>%
  group_by(news_provider) %>%
  summarise(
    mean_engagement = mean(engagement_score),
    total_articles = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_engagement))

# Display all providers sorted by mean engagement
print(provider_mean)

# Show the provider with highest mean engagement
cat("\nProvider with highest mean engagement:\n")
print(provider_mean[1, ])

# --- Part B: Category with Lowest Number of Articles ---
cat("\n=== Part B: Category with Lowest Article Count ===\n")

# Count articles per category
freq <- table(ds$headline_category)

# Get all categories with the lowest count (handles ties)
cat("Category/categories with lowest article count:\n")
print(freq[freq == min(freq)])
