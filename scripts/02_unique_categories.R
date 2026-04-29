# ============================================================
# 02_unique_categories.R - Q1: Unique Headline Categories (6 marks)
# ============================================================
# Question: How many unique values are there in headline_category?
# ============================================================

# Count the number of unique values in headline_category
num_unique <- length(unique(ds$headline_category))
cat("Number of unique headline categories:", num_unique, "\n")

# Display all unique category names sorted alphabetically
cat("\n=== All Unique Categories ===\n")
cat(paste(sort(unique(ds$headline_category)), collapse = "\n"))
