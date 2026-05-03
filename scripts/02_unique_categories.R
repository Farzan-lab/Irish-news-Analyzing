# ============================================================
# 02_unique_categories.R - Q1: Unique Headline Categories (6 marks)
# ============================================================
# Question: How many unique values are there in headline_category?
# ============================================================

# Count the number of unique values in headline_category
num_unique <- length(unique(ds$headline_category))
cat("Number of unique headline categories:", num_unique, "\n")
# Count the number of unique values in headline_category column
length(unique(ds$headline_category))


cat(paste(sort(unique(ds$headline_category)), collapse = "\n"))


# Count the number of unique values in headline_category
length(unique(ds$headline_category))

# Display all unique category names sorted alphabetically
sort(unique(ds$headline_category))



# ============================================================
# 02_unique_categories.R - Q1: Unique Headline Categories (6 marks)
# ============================================================
# Question: How many unique values are there in headline_category?
# ============================================================

# 1. RAW DATA ANALYSIS
# ------------------------------------------------------------
# Count the number of unique values in headline_category (Raw)
num_unique_raw <- length(unique(ds$headline_category))
cat("Number of unique headline categories (Raw):", num_unique_raw, "\n")

# Display all unique category names sorted alphabetically (Raw)
cat("Full list of unique categories (Raw):\n")
print(sort(unique(ds$headline_category)))


# 2. DATA CLEANING & RE-EVALUATION
# ------------------------------------------------------------
# As observed in the list, there are duplicates due to case sensitivity and delimiters.
# Let's create a cleaned version to get the 'True' number of unique categories.

ds_cleaned_cats <- ds %>%
  mutate(headline_category_clean = tolower(headline_category), # Convert to lowercase
         headline_category_clean = gsub("_", ".", headline_category_clean), # Unify delimiters
         headline_category_clean = trimws(headline_category_clean)) # Remove whitespace

num_unique_clean <- length(unique(ds_cleaned_cats$headline_category_clean))

cat("\n--- After Cleaning (Ignoring Case & Delimiters) ---\n")
cat("True number of unique headline categories:", num_unique_clean, "\n")

# Display cleaned unique categories
# sort(unique(ds_cleaned_cats$headline_category_clean))


# 3. SUMMARY FOR REPORT
# ------------------------------------------------------------
# Final Answer for Q1:
cat("\nFinal Count for Question 1:", num_unique_raw, "raw categories found.\n")