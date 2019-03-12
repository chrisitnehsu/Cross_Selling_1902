data <- SMOTE(if_ticket_success~., data = all_data_ticket, perc.over = 600, perc.under = 180)
summary(data$if_ticket_success) 
sum(data$if_ticket_success == "success") / nrow(data)
