#prepare data
set.seed(12)
train_index <- createFolds(all_data_ticket$if_ticket_success, k = 4, returnTrain = T)
train_data <- all_data_ticket[train_index[[1]],]
test_data <- all_data_ticket[-train_index[[1]],]

modeling_data <- function_engineering(train_data)
processed_test_data <- function_engineering(test_data)


# model_list <- c("svmRadial", "nnet", "kknn", "naive_bayes", "rf", "glm")
model_list <- c("svmRadial",  "glm")

ctrl <- trainControl(
  method = "repeatedcv", number = 4,repeats = 1,
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "final")

models <- caretList(if_ticket_success~., data=modeling_data, trControl=ctrl, methodList=model_list, metric = "ROC")

stack_ctrl <- trainControl(
  method = "repeatedcv", number = 4,repeats = 1,
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "final")


stack_model <- caretStack(models, method="rpart", metric = "ROC", trControl=stack_ctrl)

pred <- data.frame(svmRadial = predict(models[["svmRadial"]], processed_test_data,  type = "prob") %>% .[,"success"],
                   # nnet = predict(models[["nnet"]], processed_test_data,  type = "prob") %>% .[,"success"],
                   # kknn = predict(models[["kknn"]], processed_test_data,  type = "prob") %>% .[,"success"],
                   # naive_bayes = predict(models[["naive_bayes"]], processed_test_data,  type = "prob") %>% .[,"success"],
                   # rf = predict(models[["rf"]], processed_test_data,  type = "prob") %>% .[,"success"],
                   glm = predict(models[["glm"]], processed_test_data,  type = "prob") %>% .[,"success"],
                   stack = predict(stack_model, processed_test_data,  type = "prob"))


AUC <- colAUC(pred, processed_test_data[["if_ticket_success"]])
AUC

pred <- predict(stack_model, processed_test_data,  type = "raw")
conf <- confusionMatrix(pred, processed_test_data$if_ticket_success, mode = "everything",
                        positive = "success");conf

correlations <- cor(pred, use="everything") #相關係數矩陣做出來

corrplot::corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank") 
