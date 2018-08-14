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

train_xgb <- full_df[train_indexes, ]
train_xgb$TARGET <- target

test_xgb <- full_df[-train_indexes, ]


# Variables without predictor
vars <- names(train_xgb)[-length(names(train_xgb))]

# First: Using designTreatmentsZ
set.seed(1234)
train_treatZ <- designTreatmentsZ(train_xgb, vars)

(scoreFrameZ <- train_treatZ %>%
    use_series(scoreFrame) %>%
    select(varName, origName, code))

(newvarsZ <- scoreFrameZ %>%
    filter(code %in% c("clean", "lev")) %>%
    use_series(varName))

(train_prepZ <- prepare(train_treatZ, train_xgb, varRestriction = newvarsZ))

(test_prepZ <- prepare(train_treatZ, test_xgb, varRestriction = newvarsZ))

# cv_z <- xgb.cv(data = as.matrix(train_prepZ), label = train_xgb$TARGET,
#                nrounds = 100, nfold = 5, objective = "reg:linear",
#                eta = 0.3, max_depth = 10, early_stopping_rounds = 10,
#                verbose = 0)
# 
# elog_z <- cv_z$evaluation_log
# 
# elog_z %>% 
#   summarize(ntrees.train = which.min(train_rmse_mean), 
#             ntrees.test  = which.min(test_rmse_mean))

p_xgb <- list(booster = "gbtree", objective = "binary:logistic", eta = 0.3,
          max_depth = 8, min_child_weight = 1, gamma = 0,
          subsample = 1, nthread = 4, colsample_bytree = 1)

# Model
set.seed(1234)
model_xgb_z <- xgboost(data = as.matrix(train_prepZ), params = p_xgb,
                       label = train_xgb$TARGET,
                       nrounds = 2000, metrics = "auc",
                       early_stopping_rounds = 300, print_every_n = 50)

# Prediction on test data
test_pred <- predict(model_xgb_z, as.matrix(test_prepZ))

# prediction
read_csv("F:\\R_projects\\HomeCredit_files\\sample_submission.csv") %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = test_pred) %>%
  write_csv(paste0("tidy_xgb_", round(model_xgb_z$best_score, 4), ".csv"))



