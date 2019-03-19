set.seed(12)   
#train data 為所有資料
train_data <- all_data_ticket
modeling_data <- function_engineering(train_data)

#training models

model <- train(
  if_ticket_success ~ ., 
  data = modeling_data,
  method = "svmRadial",
  trControl = trainControl(
    method = "none"
  ), tuneGrid = data.frame(sigma = 0.01405888,  C = 1)
)

#unseen data 定義2中不在train data 的資料, 全都是失敗組, 
unseen_data <- function_engineering_pre(unseen_data)
# unseen_data <- unseen_data[names(unseen_data) %in% names(all_data_ticket)]

target_var_unseen <- unseen_data[c("if_ticket_success")] 
mice_model <- mice(unseen_data[-1], m=1, maxit = 5, seed = 50)
unseen_data <- cbind(target_var_unseen, complete(mice_model,1))

processed_test_data <- function_engineering(unseen_data)


  
#evaluation
pred <- predict(model, processed_test_data,  type = "raw")
conf <- confusionMatrix(pred, processed_test_data$if_ticket_success, mode = "everything",
                        positive = "success")

pred <- predict(model, processed_test_data,  type = "prob")
AUC <- colAUC(pred, processed_test_data[["if_ticket_success"]])
