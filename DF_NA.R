na_count <- sapply(sum_bureau, function(x) sum(length(which(is.na(x)))))
na_percentage <- sapply(sum_bureau, function(x) format(100 * sum(length(which(is.na(x))))

                                                                                                               / nrow(sum_bureau), scientific = FALSE))
features <- c(colnames(sum_bureau))
na_percentage <- round(as.numeric(na_percentage), 4)
df_na <- data.frame(na_count, na_percentage)

rm(na_count, na_percentage)
