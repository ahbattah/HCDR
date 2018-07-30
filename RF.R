
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
comm_cols <- which(grepl("mean.|sum.", names(full_df)))
full_df <- full_df %>% 
  select(-comm_cols)

# Remove variables with missing data > 70%
cols_to_remove <- df_na %>% filter(na_percentage >= 70) %>%
  select(rows)
cols_to_remove <- as.vector(cols_to_remove[['rows']])
full_df <- full_df[, names(full_df)[!(names(full_df) %in% cols_to_remove)]]



