data <- train_data
data <- SMOTE(if_ticket_success~., data = data, perc.over = 1000, perc.under = 400)
sum(data$if_ticket_success == "success") / nrow(data)
sum(data$if_ticket_success == "faliure") / nrow(data)

#全部樣本3445, 成功449, 失敗2996
#訓練樣本原2583, 成功336, 失敗2247
#測試樣本862, 成功113, 失敗749