# Irish News Dataset Analysis

## Overview
This project provides a comprehensive R-based analysis of an Irish news dataset containing articles from multiple news providers. The analysis covers engagement scores, headline categories, temporal trends, external factors, and their interrelationships across 20+ years of data.

## Dataset
The dataset contains the following columns:

| Column | Type | Description |
|--------|------|-------------|
| `publish_date` | chr | Date of publication (e.g., "Saturday, 14th of November, 2009") |
| `headline_category` | chr | Hierarchical category (e.g., news.law.courts.circuit-court) |
| `headline_text` | chr | The headline text |
| `news_provider` | chr | News source (e.g., Irish Times, RTE News) |
| `engagement_score` | dbl | Reader engagement score (some NA values) |

## Project Structure

```
irish-news-analysis/
├── README.md
├── .gitignore
├── data/
│   └── README_DATA.md          # Instructions for downloading external datasets
├── scripts/
│   ├── 01_setup.R              # Library loading and data preparation
│   ├── 02_unique_categories.R  # Q1: Unique headline categories
│   ├── 03_highest_engagement.R # Q2: Highest mean engagement & lowest articles
│   ├── 04_article_counts.R     # Q3: Article counts with depth analysis
│   ├── 05_max_engagement.R     # Q4: Max engagement year & largest increase
│   ├── 06_factors_analysis.R   # Q5: Comprehensive engagement factor investigation
│   ├── 07_correlation.R        # Q6: Correlation & trend analysis
│   ├── 08_period_analysis.R    # Q7: Period-based analysis (2019-2020)
│   ├── 09_rte_september.R      # Q8: RTE News September trend
│   └── 10_external_factors.R   # Q9: External factors investigation (deep analysis)
├── report/
│   └── Irish_News_Dataset_Analysis_Report.docx
└── .gitignore
```

## Questions Addressed

1. **Q1** (6 marks): Count unique values in `headline_category`
2. **Q2** (4 marks): Provider with highest mean engagement & category with lowest articles
3. **Q3** (3 marks): Article counts by category/provider with `tapply` + depth analysis
4. **Q4** (5 marks): Year of max engagement for TheJournal.ie & provider with largest increase
5. **Q5** (5 marks): Multi-factor engagement investigation (9 sections, ANOVA, regression, interactions)
6. **Q6** (4 marks): Correlation analysis (6A) and trend analysis (6B) per provider
7. **Q7** (3 marks): Period-based quarterly analysis for 2019-2020
8. **Q8** (4 marks): RTE News September article trend over the years
9. **Q9** (7 marks): External factors investigation (14 sections, structural breaks, lagged effects, competition)

## NA Handling Strategy
The dataset contains NA values in `engagement_score`. Each question uses an appropriate strategy:
- **Removal** (`filter(!is.na())`) for questions computing max/min (Q2, Q4)
- **na.rm = TRUE** for mean calculations within groups (Q4, Q6)
- **Complete Case Analysis** for comprehensive multi-test analyses (Q5)
- **inner_join** for merging with external datasets to avoid unmatched NA (Q9)

## Requirements

### R Packages
```r
install.packages(c("dplyr", "lubridate", "ggplot2", "tidyr"))
```

## How to Run

1. Place your dataset CSV file in the project root
2. Update the file path in `scripts/01_setup.R`
3. Run each script in order:
```r
source("scripts/01_setup.R")
source("scripts/02_unique_categories.R")
# ... and so on
```

## External Data Sources

| Source | Indicator | URL |
|--------|-----------|-----|
| World Bank | Internet Users (% of population) | https://data.worldbank.org/indicator/IT.NET.USER.ZS?locations=IE |
| World Bank | GDP (current USD) | https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?locations=IE |
| Wikipedia | Major Irish Events (2000s) | https://en.wikipedia.org/wiki/2000s_in_Ireland |
| Wikipedia | Major Irish Events (2010s) | https://en.wikipedia.org/wiki/2010s_in_Ireland |

## Key Analytical Techniques Used
- Descriptive statistics (mean, median, SD, IQR)
- ANOVA and Tukey HSD post-hoc tests
- Multiple linear regression with standardized coefficients
- Pearson correlation analysis
- Chi-squared test for NA distribution
- Structural break analysis (2008, 2020)
- Lagged effect analysis (t-1)
- Non-linear trend detection (quadratic models)
- Year-over-year growth rate and volatility
- Market share and provider competition analysis
- Dual-axis visualizations

## License
This project is for academic purposes.
