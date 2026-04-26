# ============================================================
# 10_external_factors.R - Q9: External Factors Investigation
# ============================================================
# Question: Investigate factors influencing yearly article trends
#           using original and external datasets (20+ years).
# ============================================================

# --- Step 1: Select providers with at least 20 years of data ---
provider_years <- ds %>%
  group_by(news_provider) %>%
  summarise(
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE),
    year_span = max_year - min_year + 1,
    .groups = "drop"
  ) %>%
  filter(year_span >= 20)

cat("=== Providers with 20+ years of data ===\n")
print(provider_years)

# Filter dataset for selected providers
selected_providers <- provider_years$news_provider
ds_selected <- ds %>% filter(news_provider %in% selected_providers)

# --- Step 2: Yearly article counts per provider ---
yearly_articles <- ds_selected %>%
  group_by(news_provider, year) %>%
  summarise(total_articles = n(), .groups = "drop")

# Visualize yearly trends
ggplot(yearly_articles, aes(x = year, y = total_articles, color = news_provider)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(title = "Yearly Article Count by News Provider",
       x = "Year", y = "Total Articles", color = "Provider") +
  theme_minimal()

# --- Step 3: Create top-level category for cleaner analysis ---
# Group 80+ subcategories into main categories (business, news, sport, etc.)
ds_selected$main_category <- sapply(
  strsplit(ds_selected$headline_category, "\\."), function(x) x[1]
)

cat("\n=== Main Categories ===\n")
print(sort(table(ds_selected$main_category), decreasing = TRUE))

# --- Step 4: Category composition over time ---
category_yearly <- ds_selected %>%
  group_by(news_provider, year, main_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(news_provider, year) %>%
  mutate(proportion = count / sum(count))

# Stacked area chart showing content mix changes
ggplot(category_yearly, aes(x = year, y = proportion, fill = main_category)) +
  geom_area() +
  facet_wrap(~news_provider) +
  labs(title = "Content Category Composition Over Time",
       x = "Year", y = "Proportion", fill = "Main Category") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))

# --- Step 5: Major Irish Events Timeline ---
# Sources:
# https://en.wikipedia.org/wiki/2000s_in_Ireland
# https://en.wikipedia.org/wiki/2010s_in_Ireland
events <- data.frame(
  year = c(2001, 2008, 2010, 2011, 2015, 2016, 2018, 2020),
  event = c("9/11 Attacks",
            "Financial Crisis",
            "IMF Bailout Ireland",
            "General Election",
            "Marriage Equality Ref.",
            "Brexit Vote",
            "Abortion Referendum",
            "COVID-19 Pandemic")
)

# Plot article trends with event markers
ggplot(yearly_articles, aes(x = year, y = total_articles, color = news_provider)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_vline(data = events, aes(xintercept = year), linetype = "dashed", alpha = 0.4) +
  geom_text(data = events,
            aes(x = year, y = max(yearly_articles$total_articles) * 0.95, label = event),
            angle = 90, vjust = -0.5, size = 2.5, inherit.aes = FALSE) +
  labs(title = "Yearly Articles with Major Events Overlay",
       x = "Year", y = "Total Articles", color = "Provider") +
  theme_minimal()

# --- Step 6: Trend Analysis using Linear Regression ---
trend_models <- yearly_articles %>%
  group_by(news_provider) %>%
  summarise(
    slope = coef(lm(total_articles ~ year))[2],
    r_squared = summary(lm(total_articles ~ year))$r.squared,
    p_value = summary(lm(total_articles ~ year))$coefficients[2, 4],
    .groups = "drop"
  )

cat("\n=== Trend Analysis Results ===\n")
print(trend_models)

# --- Step 7: Engagement Score vs Article Volume ---
engagement_vs_volume <- ds_selected %>%
  filter(!is.na(engagement_score)) %>%
  group_by(news_provider, year) %>%
  summarise(
    total_articles = n(),
    mean_engagement = mean(engagement_score),
    .groups = "drop"
  )

ggplot(engagement_vs_volume, aes(x = mean_engagement, y = total_articles, color = news_provider)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Mean Engagement Score vs Article Volume",
       x = "Mean Engagement Score", y = "Total Articles", color = "Provider") +
  theme_minimal()

cat("\n=== Engagement vs Volume Correlation ===\n")
engagement_cor <- engagement_vs_volume %>%
  group_by(news_provider) %>%
  summarise(
    correlation = cor(total_articles, mean_engagement, use = "complete.obs"),
    p_value = cor.test(total_articles, mean_engagement)$p.value,
    .groups = "drop"
  )
print(engagement_cor)

# --- Step 8: Year-over-Year Growth Rate ---
growth_rate <- yearly_articles %>%
  group_by(news_provider) %>%
  arrange(year) %>%
  mutate(
    yoy_change = total_articles - lag(total_articles),
    yoy_pct = (yoy_change / lag(total_articles)) * 100
  )

# Remove NA and Inf values before plotting
growth_rate_clean <- growth_rate %>%
  filter(!is.na(yoy_pct) & is.finite(yoy_pct))

ggplot(growth_rate_clean, aes(x = year, y = yoy_pct, color = news_provider)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Year-over-Year Growth Rate in Articles",
       x = "Year", y = "Growth Rate (%)", color = "Provider") +
  theme_minimal()

# --- Step 9: External Data - Irish Internet Penetration ---
# Source: https://data.worldbank.org/indicator/IT.NET.USER.ZS?locations=IE
internet_data <- data.frame(
  year = 2000:2023,
  internet_pct = c(17.9, 23.0, 27.5, 33.4, 37.0, 41.6,
                   54.8, 61.2, 63.6, 67.4, 69.8, 76.9,
                   79.7, 80.1, 80.6, 84.8, 85.0, 87.5,
                   89.0, 90.2, 92.1, 93.5, 95.0, 96.2)
)

# Merge with yearly article counts using inner_join to avoid NA warnings
merged_internet <- yearly_articles %>%
  inner_join(internet_data, by = "year")

ggplot(merged_internet, aes(x = internet_pct, y = total_articles, color = news_provider)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Internet Penetration vs Article Volume",
       subtitle = "Source: World Bank (IT.NET.USER.ZS)",
       x = "Internet Users (% of Population)", y = "Total Articles", color = "Provider") +
  theme_minimal()

cat("\n=== Internet Penetration vs Article Volume ===\n")
internet_cor <- merged_internet %>%
  group_by(news_provider) %>%
  summarise(
    correlation = cor(total_articles, internet_pct, use = "complete.obs"),
    p_value = cor.test(total_articles, internet_pct)$p.value,
    .groups = "drop"
  )
print(internet_cor)

# --- Step 10: External Data - Irish GDP ---
# Source: https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?locations=IE
gdp_data <- data.frame(
  year = 2000:2023,
  gdp_billion = c(96.6, 104.0, 121.9, 153.7, 181.5, 201.8,
                  222.0, 261.3, 264.0, 227.2, 218.5, 233.8,
                  224.7, 239.4, 258.0, 283.7, 299.2, 333.7,
                  382.5, 398.5, 425.9, 498.1, 529.2, 545.0)
)

merged_gdp <- yearly_articles %>%
  inner_join(gdp_data, by = "year")

ggplot(merged_gdp, aes(x = gdp_billion, y = total_articles, color = news_provider)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Irish GDP vs Article Volume",
       subtitle = "Source: World Bank (NY.GDP.MKTP.CD)",
       x = "GDP (Billion USD)", y = "Total Articles", color = "Provider") +
  theme_minimal()

cat("\n=== GDP vs Article Volume ===\n")
gdp_cor <- merged_gdp %>%
  group_by(news_provider) %>%
  summarise(
    correlation = cor(total_articles, gdp_billion, use = "complete.obs"),
    p_value = cor.test(total_articles, gdp_billion)$p.value,
    .groups = "drop"
  )
print(gdp_cor)

# --- Step 11: Summary of All External Factor Correlations ---
cat("\n=== Summary: All External Factors ===\n")
summary_table <- bind_rows(
  internet_cor %>% mutate(factor = "Internet Penetration"),
  gdp_cor %>% mutate(factor = "GDP"),
  engagement_cor %>% mutate(factor = "Engagement Score")
)
print(summary_table %>% select(news_provider, factor, correlation, p_value))
