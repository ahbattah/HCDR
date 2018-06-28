
# Read data
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)

bureau_balance <- read_csv("F:\\R_projects\\HomeCredit_files\\bureau_balance.csv") 
bureau <- read_csv("F:\\R_projects\\HomeCredit_files\\bureau.csv")
cc_balance <- read_csv("F:\\R_projects\\HomeCredit_files\\credit_card_balance.csv")
payments <- read_csv("F:\\R_projects\\HomeCredit_files\\installments_payments.csv") 
pc_balance <- read_csv("F:\\R_projects\\HomeCredit_files\\POS_CASH_balance.csv")
previous <- read_csv("F:\\R_projects\\HomeCredit_files\\previous_application.csv")
train <- read_csv("F:\\R_projects\\HomeCredit_files\\application_train.csv") 
test <- read_csv("F:\\R_projects\\HomeCredit_files\\application_test.csv")
