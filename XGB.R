
# Functions to be applied
fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

# Bureau DFs
sum_bbalance <- bureau_balance %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  group_by(SK_ID_BUREAU)
  summarise_all(fn)

remove(bureau_balance)
gc()

sum_bureau <- bureau %>%
  left_join(sum_bbalance, by = "SK_ID_BUREAU") %>%
  select(-SK_ID_BUREAU) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fn)

remove(bureau, sum_bbalance)
gc()

# # Credit card
sum_cc_balance <- cc_balance %>%
  select(-SK_ID_PREV) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fn)

remove(cc_balance)
gc()

# # Installments payments
sum_payments <- payments %>%
  select(-SK_ID_PREV) %>%
  mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
         PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
         DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
         DPD = ifelse(DPD > 0, DPD, 0),
         DBD = ifelse(DBD > 0, DBD, 0)) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fn)

remove(payments)
gc()

# # POS CASH balance
sum_pc_balance <- pc_balance %>%
  select(-SK_ID_PREV) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fn)

remove(pc_balance);
gc()

# # Previous applicatio
sum_prev <- previous %>%
  select(-SK_ID_PREV) %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
         DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
         DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
         DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
         DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
         APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>%
  group_by(SK_ID_CURR) %>%
  summarise_all(fn)

remove(previous)
gc()

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
  mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% 
  data.matrix()

remove(sum_bureau, sum_cc_balance, sum_payments,
       sum_pc_balance, sum_prev)

# Data partrition
dtest <- xgb.DMatrix(full_df[-train_indexes, ])
full_df <- full_df[train_indexes, ]
train_indexes <- caret::createDataPartition(target, p = .7, list = FALSE)  %>%
  c()
dtrain <- xgb.DMatrix(full_df[train_indexes, ], label = target[train_indexes])
dval <-   xgb.DMatrix(full_df[-train_indexes, ], label = target[-train_indexes])
cols <- colnames(full_df)  

p <- list(objective = "binary:logistic", eta = 0.03,
          max_depth = 10, nthread = 10)


set.seed(1234)
cv_train <- xgb.cv(data = dtrain, params = p, nfold = 2,
                   nrounds = 4, 
                   #print_every_n = 50,
                   metrics = "auc", early_stopping_rounds = 100)

m_xgb <- xgb.train(p, dtrain, nrounds = 2000, n)
  

