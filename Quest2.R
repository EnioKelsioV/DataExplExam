# Load required libraries
library(dplyr)
library(stringr)
library(ggplot2)

# Original dataset
data <- data.frame(
  CustomerID = c("A0001", "A0002", "A0003", "A0004", "A0005", "A0006", "A0007", "A0008", "A0009", "A0010"),
  PostCode = c("48", "10001F", "10001F", "M5H 2N2", "651101", "B95 A24", "D45 67", "D92 A14", "K75 N62", "1000234"),
  Gender = c("M", "F", "F", "M", "F", "F", "F", "M", "M", "M"),
  Income = c(-68000, 700000, 0, 500000, 58888, 68999, 12000, 505, 10000000, 48000),
  Age = c(117, 40, 102, 67, 41, 53, 0, "S", 20, 34),
  MaritalStatus = c("M", "W", "S", "S", "D", "D", "S", "M", "D", "D"),
  TransactionAmount = c(3500, 6000, 10000, 400, 1000, 6500, 5500, 3000, 1300, 6)
)

# ----------------------------
# Data Cleaning
# ----------------------------
cat("\nCleaning Data:\n")

# Correct invalid Income (remove negative and zero values)
data <- data %>% mutate(Income = ifelse(Income <= 0, NA, Income))

# Replace invalid Age values
data <- data %>%
  mutate(Age = as.numeric(Age)) %>% # Convert to numeric
  mutate(Age = ifelse(Age < 18 | Age > 100, NA, Age)) # Replace invalid ages with NA

# Clean PostCode (ensure consistent alphanumeric format)
data <- data %>%
  mutate(PostCode = str_replace_all(PostCode, " ", "")) # Remove spaces for consistency

# Remove rows with NA in critical columns (Age, Income)
data <- na.omit(data)

# ----------------------------
# Add 5 Random Rows
# ----------------------------
# Generate 5 new random rows
set.seed(123) # For reproducibility
new_rows <- data.frame(
  CustomerID = paste0("A", sprintf("%04d", 11:15)),
  PostCode = c("H7K3M4", "20002", "L4Z2M5", "T1A2B3", "V5H4N6"),
  Gender = sample(c("M", "F"), 5, replace = TRUE),
  Income = sample(seq(30000, 100000, by = 1000), 5, replace = TRUE),
  Age = sample(20:65, 5, replace = TRUE),
  MaritalStatus = sample(c("M", "W", "S", "D"), 5, replace = TRUE),
  TransactionAmount = sample(seq(500, 7000, by = 500), 5, replace = TRUE)
)

# Combine the original cleaned dataset with new rows
final_data <- rbind(data, new_rows)

# ----------------------------
# Data Summary
# ----------------------------
cat("\nFinal Dataset Summary:\n")
print(summary(final_data))