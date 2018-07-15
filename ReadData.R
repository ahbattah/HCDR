
# Read data
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, xgboost)

# Work
bureau_balance <- read_csv("F:\\R_projects\\HomeCredit_files\\bureau_balance.csv") 
bureau <- read_csv("F:\\R_projects\\HomeCredit_files\\bureau.csv")
cc_balance <- read_csv("F:\\R_projects\\HomeCredit_files\\credit_card_balance.csv")
payments <- read_csv("F:\\R_projects\\HomeCredit_files\\installments_payments.csv") 
pc_balance <- read_csv("F:\\R_projects\\HomeCredit_files\\POS_CASH_balance.csv")
previous <- read_csv("F:\\R_projects\\HomeCredit_files\\previous_application.csv")
train <- read_csv("F:\\R_projects\\HomeCredit_files\\application_train.csv") 
test <- read_csv("F:\\R_projects\\HomeCredit_files\\application_test.csv")


# Home
bureau_balance <- read_csv("E:\\R\\files\\HomeCredit\\bureau_balance.csv") 
bureau <- read_csv("E:\\R\\files\\HomeCredit\\bureau.csv")
cc_balance <- read_csv("E:\\R\\files\\HomeCredit\\credit_card_balance.csv")
payments <- read_csv("E:\\R\\files\\HomeCredit\\installments_payments.csv") 
pc_balance <- read_csv("E:\\R\\files\\HomeCredit\\POS_CASH_balance.csv")
previous <- read_csv("E:\\R\\files\\HomeCredit\\previous_application.csv")
train <- read_csv("E:\\R\\files\\HomeCredit\\application_train.csv") 
test <- read_csv("E:\\R\\files\\HomeCredit\\application_test.csv")

# NA count and percentage
# na_count <- sapply(train, function(x) sum(length(which(is.na(x)))))
# na_percentage <- sapply(train, function(x) format(100 * sum(length(which(is.na(x)))) 
#                         / nrow(train), scientific = FALSE))
# na_percentage <- round(as.numeric(na_percentage), 4)
# na_count_df <- data.frame(na_count, na_percentage)



