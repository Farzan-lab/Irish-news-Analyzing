# External Data Sources

This folder should contain the external datasets used in Question 9.
These files are not included in the repository due to licensing. Please download them manually.

## Required Downloads

### 1. Irish Internet Penetration
- **Source**: World Bank
- **Indicator**: Individuals using the Internet (% of population)
- **URL**: https://data.worldbank.org/indicator/IT.NET.USER.ZS?locations=IE
- **Steps**: Click "Download" → "CSV" → Save as `irish_internet_penetration.csv`

### 2. Irish GDP
- **Source**: World Bank
- **Indicator**: GDP (current US$)
- **URL**: https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?locations=IE
- **Steps**: Click "Download" → "CSV" → Save as `irish_gdp.csv`

## File Structure After Download
```
data/
├── README_DATA.md (this file)
├── irish_internet_penetration.csv
└── irish_gdp.csv
```

## Note
The R scripts in `scripts/10_external_factors.R` contain hardcoded approximate values
as a fallback. For accurate results, download the actual CSV files and uncomment
the `read.csv()` lines in the script.
