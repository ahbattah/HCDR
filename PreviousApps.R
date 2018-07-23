# POS_CASH_balance

# 1. POS_CASH_balance
# NA count and percentage
na_count <- sapply(pc_balance, function(x) sum(length(which(is.na(x)))))
na_percentage <- sapply(pc_balance, function(x) format(100 * sum(length(which(is.na(x))))
                                                       / nrow(pc_balance), scientific = FALSE))
na_percentage <- round(as.numeric(na_percentage), 4)
pc_balance_na <- data.frame(na_count, na_percentage)

rm(na_count, na_percentage)

# Data Imputation
pc_balance$CNT_INSTALMENT[is.na(pc_balance$CNT_INSTALMENT)] <- median(
  pc_balance$CNT_INSTALMENT, na.rm = TRUE)

pc_balance$CNT_INSTALMENT_FUTURE[is.na(pc_balance$CNT_INSTALMENT_FUTURE)] <- 
  median(pc_balance$CNT_INSTALMENT_FUTURE, na.rm = TRUE)

pc_balance <- pc_balance %>% select(-SK_ID_PREV)

remove(pc_balance_na, pc_balance_na)

# 2. Previous
na_count <- sapply(previous, function(x) sum(length(which(is.na(x)))))
na_percentage <- sapply(previous, function(x) format(100 * sum(length(which(is.na(x))))
                                                       / nrow(previous), scientific = FALSE))
na_percentage <- round(as.numeric(na_percentage), 4)
previous_na <- data.frame(na_count, na_percentage)

rm(na_count, na_percentage)

# Data Imputation
# Remove columns with missing values > 5%
previous <- previous %>% 
  mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
         DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
         DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
         DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
         DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION))

previous <- previous %>% 
  select(-c(SK_ID_PREV, AMT_ANNUITY, AMT_DOWN_PAYMENT, AMT_GOODS_PRICE,
            starts_with("RATE"), NAME_TYPE_SUITE,CNT_PAYMENT,
            starts_with("DAYS"), NFLAG_INSURED_ON_APPROVAL))

previous$AMT_CREDIT[is.na(previous$AMT_CREDIT)] <- median(
  previous$AMT_CREDIT, na.rm = TRUE)

previous$PRODUCT_COMBINATION <- as.factor(previous$PRODUCT_COMBINATION)
previous$PRODUCT_COMBINATION <- `levels<-`(addNA(previous$PRODUCT_COMBINATION), 
                                 c(levels(previous$PRODUCT_COMBINATION), 
                                   "POS household with interest"))



remove(pc_balance_na, previous_na)
