data <- train_data
data <- SMOTE(target_var~., data = data, perc.over = 200, perc.under = 200)
sum(data$target_var == "success") / nrow(data)
sum(data$target_var == "faliure") / nrow(data)

