
# Credit card

# 1. Credit card balance
# NA count and percentage
na_count <- sapply(cc_balance, function(x) sum(length(which(is.na(x)))))
na_percentage <- sapply(cc_balance, function(x) format(100 * sum(length(which(is.na(x))))
                                                        / nrow(cc_balance), scientific = FALSE))
na_percentage <- round(as.numeric(na_percentage), 4)
cc_balance_na <- data.frame(na_count, na_percentage)

rm(na_count, na_percentage)

# Data Imputation
# Remove columns with missing values > 5%
cc_balance <- cc_balance %>% 
  select(-c(AMT_DRAWINGS_ATM_CURRENT, AMT_DRAWINGS_OTHER_CURRENT,
            AMT_DRAWINGS_POS_CURRENT, AMT_INST_MIN_REGULARITY,
            AMT_PAYMENT_CURRENT, CNT_DRAWINGS_ATM_CURRENT,
            CNT_DRAWINGS_POS_CURRENT, CNT_INSTALMENT_MATURE_CUM,
            CNT_DRAWINGS_OTHER_CURRENT, SK_ID_PREV))


# 2. installments payment
na_count <- sapply(payments, function(x) sum(length(which(is.na(x)))))
na_percentage <- sapply(payments, function(x) format(100 * sum(length(which(is.na(x))))
                                                       / nrow(payments), scientific = FALSE))
na_percentage <- round(as.numeric(na_percentage), 4)
payments_na <- data.frame(na_count, na_percentage)

rm(na_count, na_percentage)

# Data Imputation
payments$DAYS_ENTRY_PAYMENT[is.na(payments$DAYS_ENTRY_PAYMENT)] <- median(
  payments$DAYS_ENTRY_PAYMENT, na.rm = TRUE)

payments$AMT_PAYMENT[is.na(payments$AMT_PAYMENT)] <- median(
  payments$AMT_PAYMENT, na.rm = TRUE)

payments <- payments %>% select(-SK_ID_PREV)
remove(cc_balance_na, payments_na)


