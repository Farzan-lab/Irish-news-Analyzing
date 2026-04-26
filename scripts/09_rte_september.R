# ============================================================
# 09_rte_september.R - Q8: RTE News September Trend
# ============================================================
# Question: Examine the trend in articles published by RTE News
#           in September over the years. Create an appropriate chart.
# ============================================================

# Filter for RTE News articles published in September (month == 9)
rte_september <- ds %>%
  filter(news_provider == "RTE News", month == 9)

# Count total articles per year for September
rte_sept_yearly <- rte_september %>%
  group_by(year) %>%
  summarise(total_articles = n(), .groups = "drop")

cat("=== RTE News September Article Counts by Year ===\n")
print(rte_sept_yearly)

# Line chart with trend line
ggplot(rte_sept_yearly, aes(x = year, y = total_articles)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "grey") +
  labs(title = "Trend in Number of Articles Published by RTE News in September",
       x = "Year",
       y = "Total Number of Articles") +
  scale_x_continuous(breaks = seq(min(rte_sept_yearly$year),
                                   max(rte_sept_yearly$year), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
