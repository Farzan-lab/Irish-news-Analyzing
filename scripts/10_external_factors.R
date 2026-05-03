# ============================================================
# 10_external_factors_SIMPLE.R - Q5: External Factors Analysis
# ============================================================
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

# --- SECTION 1: DATA PREPARATION ---
# Parse dates and extract year
ds$date_parsed <- dmy(gsub("(\\d+)(st|nd|rd|th)", "\\1", ds$publish_date))
ds$year <- year(ds$date_parsed)

# Filter providers with at least 20 years of data for long-term analysis
provider_filter <- ds %>%
  group_by(news_provider) %>%
  summarise(year_span = max(year) - min(year) + 1) %>%
  filter(year_span >= 20)

ds_selected <- ds %>% filter(news_provider %in% provider_filter$news_provider)

# Aggregate yearly article counts per provider
yearly_articles <- ds_selected %>%
  group_by(news_provider, year) %>%
  summarise(total_articles = n(), .groups = "drop")

# --- SECTION 2: EXTERNAL DATA (GDP) ---
# Load Irish GDP data (Source: World Bank)
gdp_raw <- read.csv("C:/Users/farza/Job/Github/Irish-news-Analyzing/data/irish data.csv", skip = 4)

gdp_data <- gdp_raw %>%
  filter(Country.Code == "IRL") %>%
  select(starts_with("X")) %>%
  tidyr::pivot_longer(everything(), names_to = "year", values_to = "gdp_usd") %>%
  mutate(year = as.numeric(gsub("X", "", year)), 
         gdp_billion = gdp_usd / 1e9) %>%
  filter(!is.na(gdp_usd)) %>%
  select(year, gdp_billion)

# --- SECTION 3: IMPACT OF MAJOR EVENTS ---
# Define key historical events for visual overlay
events <- data.frame(
  year = c(2008, 2016, 2020),
  event = c("Financial Crisis", "Brexit Vote", "COVID-19")
)

ggplot(yearly_articles, aes(x = year, y = total_articles, color = news_provider)) +
  geom_line(linewidth = 1) +
  geom_vline(data = events, aes(xintercept = year), linetype = "dashed", alpha = 0.5) +
  geom_text(data = events, aes(x = year, y = max(yearly_articles$total_articles), label = event), 
            angle = 90, vjust = -0.5, size = 3, inherit.aes = FALSE) +
  labs(title = "Impact of Major Events on News Volume", 
       subtitle = "Dashed lines represent key social and economic milestones",
       x = "Year", y = "Total Articles") +
  theme_minimal()

# --- SECTION 4: GDP VS ARTICLE VOLUME CORRELATION ---
# Merge internal news data with external GDP data
merged_gdp <- yearly_articles %>% inner_join(gdp_data, by = "year")

ggplot(merged_gdp, aes(x = gdp_billion, y = total_articles, color = news_provider)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) + # Simple linear regression line
  labs(title = "News Volume vs National GDP",
       subtitle = "Assessing if economic growth correlates with article production",
       x = "Irish GDP (Billion USD)", y = "Annual Article Count") +
  theme_minimal()

# Calculate Correlation Coefficient for reporting
correlations <- merged_gdp %>%
  group_by(news_provider) %>%
  summarise(correlation_score = round(cor(total_articles, gdp_billion), 2))

print("Correlation between GDP and Article Volume:")
print(correlations)

# --- SECTION 5: CONTENT CATEGORY EVOLUTION ---
# Simplify categories to the main parent category
ds_selected$main_cat <- sapply(strsplit(as.character(ds_selected$headline_category), "\\."), `[`, 1)

cat_trend <- ds_selected %>%
  group_by(year, main_cat) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percentage = count / sum(count))

# Select top 5 categories to avoid over-cluttering the plot
top_cats <- cat_trend %>% 
  group_by(main_cat) %>% 
  summarise(total = sum(count)) %>% 
  top_n(5, total) %>% 
  pull(main_cat)

ggplot(cat_trend %>% filter(main_cat %in% top_cats), aes(x = year, y = percentage, fill = main_cat)) +
  geom_area(alpha = 0.8) +
  labs(title = "Evolution of News Topics (20 Years)", 
       subtitle = "Proportional shift in editorial focus",
       x = "Year", y = "Content Share (%)") +
  theme_minimal()