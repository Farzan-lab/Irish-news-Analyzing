# ============================================================
# 09_rte_september.R - Q8: RTE News September Trend (4 marks)
# ============================================================
# Question: Examine the trend in articles by RTE News in September.
#           Create an appropriate chart.
# ============================================================

library(dplyr)
library(lubridate)
library(ggplot2)

# Filter for RTE News articles in September (month == 9)
rte_september <- ds %>%
  filter(news_provider == "RTE News", month == 9)

# Count articles per year
rte_sept_yearly <- rte_september %>%
  group_by(year) %>%
  summarise(total_articles = n(), .groups = "drop")

cat("=== RTE News September Article Counts ===\n")
print(rte_sept_yearly)

# Line chart with trend line
ggplot(rte_sept_yearly, aes(x = year, y = total_articles)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "grey") +
  labs(title = "Trend in Number of Articles Published by RTE News in September",
       x = "Year", y = "Total Number of Articles") +
  scale_x_continuous(breaks = seq(min(rte_sept_yearly$year),
                                   max(rte_sept_yearly$year), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
