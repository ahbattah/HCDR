
# Credit card

# 1. Credit card balance
# NA count and percentage
na_count <- sapply(cc_balance, function(x) sum(length(which(is.na(x)))))
na_percentage <- sapply(cc_balance, function(x) format(100 * sum(length(which(is.na(x))))
                                                        / nrow(cc_balance), scientific = FALSE))
na_percentage <- round(as.numeric(na_percentage), 4)
cc_balance_na <- data.frame(na_count, na_percentage)

rm(na_count, na_percentage)

# 2. installments payment
na_count <- sapply(payments, function(x) sum(length(which(is.na(x)))))
na_percentage <- sapply(payments, function(x) format(100 * sum(length(which(is.na(x))))
                                                       / nrow(payments), scientific = FALSE))
na_percentage <- round(as.numeric(na_percentage), 4)
payments_na <- data.frame(na_count, na_percentage)

rm(na_count, na_percentage)
