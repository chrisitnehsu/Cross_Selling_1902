#prepare data
set.seed(12)
train_index <- createFolds(all_data_valid$if_ticket_success, k = 4, returnTrain = T)
train_data <- all_data_valid[train_index[[1]],]
test_data <- all_data_valid[-train_index[[1]],]

modeling_data <- function_engineering(train_data)
processed_test_data <- function_engineering(test_data)


model_list <- c("svmRadial", "nnet", "kknn", "naive_bayes", "rf", "glm")

#
set.seed(12)
function_train_pred <- function(model_choose){
model <- train(
  if_ticket_success ~ ., 
  data = modeling_data,
  method = model_choose,
  metric = "ROC",
  trControl = trainControl(
    method = "cv", number = 4,
    verboseIter = TRUE,
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    savePredictions = "final"))

pred <- predict(model, processed_test_data,  type = "prob")
return(pred$success)
}

t1 <- function_train_pred(model_list[1]) 
t2 <- function_train_pred(model_list[2])
t3 <- function_train_pred(model_list[3])
t4 <- function_train_pred(model_list[4])
t5 <- function_train_pred(model_list[5])
t6 <- function_train_pred(model_list[6])
t7 <- function_train_pred(model_list[7])


bag_df_soft <- cbind(t1,t2,t3,t5,t6)
bag_pred <- rowMeans(bag_df_soft)

#
threshold <- 0.35
bag_pred <- ifelse(bag_pred >= threshold, "success", "faliure") %>% factor(levels = c("success", "faliure"),labels = c("success", "faliure"))
conf <- confusionMatrix(bag_pred, processed_test_data$if_ticket_success, mode = "everything",
                        positive = "success")

#AUC
bag_pred <- predict(model, processed_test_data,  type = "prob")
AUC <- colAUC(bag_pred, processed_test_data[["if_ticket_success"]])
# AUC

metrics <- as.vector(cbind(conf$overall[["Accuracy"]]
                           ,conf$byClass[["Sensitivity"]]
                           ,conf$byClass[["Precision"]]
                           ,conf$byClass[["F1"]]
                           ,AUC[[1]])) %>% as.data.frame
rownames(metrics) <- c("Accuracy", "Sensitivity", "Precision", "F1", "ROC")
names(metrics)[1] <- "svmRadial"
metrics


# test_data_w_target <- cbind(test_data, pred)
