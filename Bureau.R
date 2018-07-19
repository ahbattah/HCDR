
# Bureau
bureau_full <- bureau %>% 
  left_join(bureau_balance, by = "SK_ID_BUREAU") %>% 
  select(-SK_ID_BUREAU)

rm(bureau, bureau_balance)

# NA count and percentage
na_count <- sapply(bureau_full, function(x) sum(length(which(is.na(x)))))
na_percentage <- sapply(bureau_full, function(x) format(100 * sum(length(which(is.na(x))))
                        / nrow(bureau_full), scientific = FALSE))
na_percentage <- round(as.numeric(na_percentage), 4)
bureau_na <- data.frame(na_count, na_percentage)

rm(na_count, na_percentage)

# DAYS_CREDIT_ENDDATE			  median
# AMT_CREDIT_MAX_OVERDUE    impute
# AMT_CREDIT_SUM				    mean
# AMT_CREDIT_SUM_DEBT			  impute
# AMT_CREDIT_SUM_LIMIT		  impute
# AMT_ANNUITY					      impute
# MONTHS_BALANCE				    median
# STATUS						        impute by X

# Data imputation
# 1. Manual
bureau_full$DAYS_CREDIT_ENDDATE[is.na(bureau_full$DAYS_CREDIT_ENDDATE)] <- 
  median(bureau_full$DAYS_CREDIT_ENDDATE, na.rm = TRUE)

bureau_full$AMT_CREDIT_SUM[is.na(bureau_full$AMT_CREDIT_SUM)] <- mean(
  bureau_full$AMT_CREDIT_SUM, na.rm = TRUE)

bureau_full$MONTHS_BALANCE[is.na(bureau_full$MONTHS_BALANCE)] <- 
  median(bureau_full$MONTHS_BALANCE, na.rm = TRUE)

bureau_full$STATUS <- as.factor(bureau_full$STATUS)
bureau_full$STATUS <- `levels<-`(addNA(bureau_full$STATUS), 
                               c(levels(bureau_full$STATUS), 
                                 "X"))

