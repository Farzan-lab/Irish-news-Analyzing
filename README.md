# Irish News Dataset Analysis

## Overview
This project analyses an Irish news dataset containing articles from multiple news providers. The analysis covers engagement scores, headline categories, temporal trends, and external factors influencing article publication.

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
├── scripts/
│   ├── 01_setup.R              # Library loading and data preparation
│   ├── 02_unique_categories.R  # Q1: Unique headline categories
│   ├── 03_highest_engagement.R # Q2: Highest mean engagement & lowest articles
│   ├── 04_article_counts.R     # Q3: Article counts by category & provider
│   ├── 05_max_engagement.R     # Q4: Max engagement year & largest increase
│   ├── 06_factors_analysis.R   # Q5: Factors associated with engagement
│   ├── 07_correlation.R        # Q6: Correlation & trend analysis
│   ├── 08_period_analysis.R    # Q7: Period-based analysis (2019-2020)
│   ├── 09_rte_september.R      # Q8: RTE News September trend
│   └── 10_external_factors.R   # Q9: External factors investigation
├── report/
│   └── Irish_News_Dataset_Analysis_Report.docx
└── .gitignore
```

## Questions Addressed

1. **Q1**: Count unique values in `headline_category`
2. **Q2**: Provider with highest mean engagement & category with lowest articles
3. **Q3**: Article counts by category/provider with statistical summary using `tapply`
4. **Q4**: Year of max engagement for TheJournal.ie & provider with largest increase
5. **Q5**: Multi-factor investigation of engagement scores (category, provider, time, headline length)
6. **Q6**: Correlation analysis (6A) and trend analysis (6B) per provider
7. **Q7**: Period-based quarterly analysis for 2019-2020
8. **Q8**: RTE News September article trend over the years
9. **Q9**: External factors (internet penetration, GDP, major events) vs article volume

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
| Wikipedia | Major Irish Events | https://en.wikipedia.org/wiki/2000s_in_Ireland |

## License
This project is for academic purposes.
