# ============================================================
# 06_factors_analysis.R - Q5: Engagement Factor Analysis (5 marks)
# ============================================================
library(dplyr)
library(ggplot2)
library(lubridate)

# Cleaning
ds_clean <- ds %>% 
  filter(!is.na(engagement_score), engagement_score >= 0)

# Create helper columns separately to avoid errors
ds_clean$headline_length <- nchar(ds_clean$headline_text)
ds_clean$date_parsed <- dmy(gsub("(\\d+)(st|nd|rd|th)", "\\1", ds_clean$publish_date))
ds_clean$year <- year(ds_clean$date_parsed)
ds_clean$month <- month(ds_clean$date_parsed, label = TRUE)
ds_clean$main_category <- sapply(strsplit(as.character(ds_clean$headline_category), "\\."), function(x) x[1])
ds_clean$main_category[is.na(ds_clean$main_category)] <- "unknown"

cat("Rows used for analysis:", nrow(ds_clean), "\n")

# ============================================================
# 1. Engagement by News Provider
# ============================================================
cat("\n=== Engagement by Provider ===\n")
provider_summary <- ds_clean %>%
  group_by(news_provider) %>%
  summarise(avg_score = mean(engagement_score), 
            median_score = median(engagement_score),
            count = n(), .groups = "drop") %>%
  arrange(desc(avg_score))
print(provider_summary)

# Bar chart: which provider gets more engagement?
ggplot(provider_summary %>% filter(count > 100), 
       aes(x = reorder(news_provider, avg_score), y = avg_score, fill = news_provider)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Engagement by News Provider",
       subtitle = "Excluding providers with fewer than 100 articles",
       x = "Provider", y = "Average Engagement Score") +
  theme_minimal() + theme(legend.position = "none")

# Boxplot: how spread is the engagement for each provider?
ggplot(ds_clean %>% filter(news_provider %in% c("Irish Times", "RTE News", 
                                                "Irish Examiner", "Irish Independent", "TheJournal.ie")),
       aes(x = reorder(news_provider, engagement_score, FUN = median),
           y = engagement_score, fill = news_provider)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Engagement Distribution by Provider",
       x = "Provider", y = "Engagement Score") +
  theme_minimal() + theme(legend.position = "none")

# ============================================================
# 2. Engagement by Main Category
# ============================================================
cat("\n=== Engagement by Main Category ===\n")
category_summary <- ds_clean %>%
  group_by(main_category) %>%
  summarise(avg_score = mean(engagement_score),
            count = n(), .groups = "drop") %>%
  filter(count > 100) %>%
  arrange(desc(avg_score))
print(category_summary)

# Bar chart: which category gets more engagement?
ggplot(category_summary, 
       aes(x = reorder(main_category, avg_score), y = avg_score, fill = main_category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Engagement by Main Category",
       subtitle = "Only categories with 100+ articles",
       x = "Category", y = "Average Engagement Score") +
  theme_minimal() + theme(legend.position = "none")

# Boxplot: distribution comparison
ggplot(ds_clean %>% filter(main_category %in% category_summary$main_category),
       aes(x = reorder(main_category, engagement_score, FUN = median),
           y = engagement_score, fill = main_category)) +
  geom_boxplot() + coord_flip() +
  labs(title = "Engagement Distribution by Category",
       x = "Category", y = "Engagement Score") +
  theme_minimal() + theme(legend.position = "none")

# ============================================================
# 3. Engagement by Headline Length
# ============================================================
cat("\n=== Headline Length vs Engagement ===\n")
correlation <- cor(ds_clean$headline_length, ds_clean$engagement_score)
cat("Correlation:", round(correlation, 4), "\n")

# Scatter plot
ggplot(ds_clean, aes(x = headline_length, y = engagement_score)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Headline Length vs Engagement Score",
       subtitle = paste("Correlation:", round(correlation, 4)),
       x = "Headline Length (characters)", y = "Engagement Score") +
  theme_minimal()

# Binned bar chart: easier to see the pattern
ds_clean$length_group <- cut(ds_clean$headline_length,
                             breaks = c(0, 30, 50, 70, 90, 120, Inf),
                             labels = c("0-30", "31-50", "51-70", "71-90", "91-120", "120+"))

length_summary <- ds_clean %>%
  group_by(length_group) %>%
  summarise(avg_score = mean(engagement_score), count = n(), .groups = "drop")

ggplot(length_summary, aes(x = length_group, y = avg_score, fill = length_group)) +
  geom_col() +
  geom_text(aes(label = paste0("n=", format(count, big.mark = ","))), vjust = -0.5, size = 3) +
  labs(title = "Average Engagement by Headline Length",
       x = "Length Range (characters)", y = "Average Score") +
  theme_minimal() + theme(legend.position = "none")

# ============================================================
# 4. Engagement Over Time
# ============================================================
cat("\n=== Yearly Trend ===\n")
yearly_summary <- ds_clean %>%
  group_by(year) %>%
  summarise(avg_score = mean(engagement_score), .groups = "drop")

ggplot(yearly_summary, aes(x = year, y = avg_score)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "grey") +
  labs(title = "Engagement Trend Over Years",
       subtitle = "Dashed line shows overall trend",
       x = "Year", y = "Average Engagement Score") +
  theme_minimal()

# Monthly pattern
monthly_summary <- ds_clean %>%
  group_by(month) %>%
  summarise(avg_score = mean(engagement_score), .groups = "drop")

ggplot(monthly_summary, aes(x = month, y = avg_score, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Monthly Seasonality of Engagement",
       subtitle = "Do certain months attract more engagement?",
       x = "Month", y = "Average Engagement Score") +
  theme_minimal()

# ============================================================
# 5. Provider Trends Over Time
# ============================================================
yearly_by_provider <- ds_clean %>%
  filter(news_provider %in% c("Irish Times", "RTE News", 
                              "Irish Examiner", "Irish Independent", "TheJournal.ie")) %>%
  group_by(news_provider, year) %>%
  summarise(avg_score = mean(engagement_score), .groups = "drop")

ggplot(yearly_by_provider, aes(x = year, y = avg_score, color = news_provider)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(title = "Engagement Trends by Provider Over Time",
       subtitle = "How each provider's engagement changed across years",
       x = "Year", y = "Average Engagement Score", color = "Provider") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ============================================================
# 6. Summary
# ============================================================
cat("\n=== KEY FINDINGS ===\n")
cat("1. Provider: Irish Times has highest engagement among major providers\n")
cat("2. Category: News categories tend to have higher engagement than lifestyle/culture\n")
cat("3. Headline Length: Correlation =", round(correlation, 4), "\n")
cat("4. Time: Engagement has been gradually increasing over the years\n")
cat("5. Monthly: Some months show slightly higher engagement\n")
