na_count <- sapply(full_df, function(x) sum(length(which(is.na(x)))))
na_percentage <- sapply(full_df, function(x) format(100 * sum(length(which(is.na(x))))
  / nrow(full_df), scientific = FALSE))

features <- c(colnames(full_df))
na_percentage <- round(as.numeric(na_percentage), 6)
df_na <- data.frame(na_count, na_percentage)

df_na <- cbind(rows = rownames(df_na), df_na)
row.names(df_na) <- NULL

rm(na_count, na_percentage)
