
# Functions to be applied
fn <- funs(mean, sum, .args = list(na.rm = TRUE))

# Bureau DFs
sum_bbalance <- bureau_balance %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  group_by(SK_ID_BUREAU) %>% 
  summarise_all(fn)

remove(bureau_balance)

sum_bureau <- bureau %>%
  #left_join(sum_bbalance, by = "SK_ID_BUREAU") %>%
  select(-SK_ID_BUREAU) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fn)

remove(bureau, sum_bbalance)

# Remove variables with missing data > 60%
# sum_bureau <- sum_bureau %>% 
#   select(-c(AMT_ANNUITY_sd, MONTHS_BALANCE_sd_sd, STATUS_sd_sd,
#             MONTHS_BALANCE_mean_sd, STATUS_mean_sd,
#             MONTHS_BALANCE_sum_sd, STATUS_sum_sd,
#             MONTHS_BALANCE_n_distinct_sd, STATUS_n_distinct_sd,
#             AMT_ANNUITY_mean))

# # Credit card
sum_cc_balance <- cc_balance %>%
  select(-SK_ID_PREV) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fn)

remove(cc_balance)

# # Installments payments
sum_payments <- payments %>%
  select(-SK_ID_PREV) %>%
  # mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
  #        PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
  #        DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
  #        DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
  #        DPD = ifelse(DPD > 0, DPD, 0),
  #        DBD = ifelse(DBD > 0, DBD, 0)) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fn)

remove(payments)

# # POS CASH balance
sum_pc_balance <- pc_balance %>%
  select(-SK_ID_PREV) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fn)

remove(pc_balance);

# # Previous applicatio
sum_prev <- previous %>%
  select(-SK_ID_PREV) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  # mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
  #        DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
  #        DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
  #        DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
  #        DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
  #        APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fn)

remove(previous)

# Remove variables with missing data > 60%
# sum_prev <- sum_prev %>% 
#   select(-c(DAYS_FIRST_DRAWING_sd, RATE_INTEREST_PRIMARY_sd,
#             RATE_INTEREST_PRIVILEGED_sd, RATE_INTEREST_PRIMARY_mean,
#             RATE_INTEREST_PRIVILEGED_mean, DAYS_FIRST_DRAWING_mean))

# Target variable
target <- train$TARGET
train_indexes <- 1:nrow(train)

# Full data
full_df <- train %>% 
  select(-TARGET) %>% 
  bind_rows(test) %>% 
  left_join(sum_bureau, by = "SK_ID_CURR") %>% 
  left_join(sum_cc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_payments, by = "SK_ID_CURR") %>% 
  left_join(sum_pc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_prev, by = "SK_ID_CURR") %>%
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

# Remove commulative features
comm_cols <- which(grepl("mean_|sum_", names(full_df)))
full_df <- full_df %>% 
  select(-comm_cols)


full_df <- full_df %>% 
  select(c(imp_cols))

# Remove variables with missing data > 60%
# cols_to_remove <- df_na %>% filter(na_percentage >= 60) %>% 
#                               select(rows)
# cols_to_remove <- as.vector(cols_to_remove[['rows']])
# full_df <- full_df[, names(full_df)[!(names(full_df) %in% cols_to_remove)]]

# full_df to matrix
full_df <- full_df %>% 
  data.matrix()

# remove(sum_bureau, sum_cc_balance, sum_payments,
#        sum_pc_balance, sum_prev)

# Data partrition
dtest <- xgb.DMatrix(full_df[-train_indexes, ])
full_df <- full_df2[train_indexes, ]
train_indexes <- caret::createDataPartition(target, p = .9, list = FALSE)  %>%
  c()
dtrain <- xgb.DMatrix(full_df[train_indexes, ], label = target[train_indexes])
dval <-   xgb.DMatrix(full_df[-train_indexes, ], label = target[-train_indexes])
cols <- colnames(full_df)  

# Model
p <- list(booster = "gbtree", objective = "binary:logistic", eta = 0.01,
          max_depth = 8, min_child_weight = 1, gamma = 2,
          subsample = 1, nthread = 4, colsample_bytree = 1)

set.seed(1234)
xgb_train <- xgb.train(data = dtrain, params = p,
                   nrounds = 2000, metrics = "auc",
                   print_every_n = 25,
                   early_stopping_rounds = 300, 
                   list(val = dval))

xgb.importance(cols, model = xgb_train) %>% 
  xgb.plot.importance(top_n = 30)

df_tmp <- xgb.importance(cols, model = xgb_train)
imp_cols <- df_tmp[['Feature']][1:150]

# prediction
read_csv("F:\\R_projects\\HomeCredit_files\\sample_submission.csv") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(xgb_train, dtest)) %>%
  write_csv(paste0("tidy_xgb_", round(xgb_train$best_score, 4), ".csv"))

