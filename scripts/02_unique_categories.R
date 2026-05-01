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