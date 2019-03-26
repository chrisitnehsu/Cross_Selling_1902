set.seed(12)
train_index <- createFolds(all_data_ticket$if_ticket_success, k = 4, returnTrain = T)

function_modeling <- function(fold, model_choose){
train_data <- all_data_ticket[train_index[[fold]],]
test_data <- all_data_ticket[-train_index[[fold]],]

modeling_data <- function_engineering(train_data)
processed_test_data <- function_engineering(test_data)

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

#test threshold-----------------------------------------------
# resample_stats <- thresholder(model,
#                               threshold = seq(0, 1, by = 0.05),
#                               final = TRUE);resample_stats


pred <- predict(model, processed_test_data,  type = "prob")
threshold <- 0.5
pred <- ifelse(pred$success >= threshold, "success", "faliure") %>% factor(levels = c("success", "faliure"),labels = c("success", "faliure"))
conf <- confusionMatrix(pred, processed_test_data$if_ticket_success, mode = "everything",
                        positive = "success");conf

#AUC
pred <- predict(model, processed_test_data,  type = "prob")
AUC <- colAUC(pred, processed_test_data[["if_ticket_success"]])
AUC

metrics <- as.vector(cbind(conf$overall[["Accuracy"]]
                           ,conf$byClass[["Sensitivity"]]
                           ,conf$byClass[["Precision"]]
                           ,conf$byClass[["F1"]]
                           ,AUC[[1]])) %>% as.data.frame
rownames(metrics) <- c("Accuracy", "Sensitivity", "Precision", "F1", "ROC")
# names(metrics)[1] <- substitute(fold)
return(metrics)
}
metric <- function_modeling(fold = 1, "svmRadial")

performance <- data.frame(V1 = c(1,1,1,1,1), row.names = c("Accuracy", "Sensitivity", "Precision", "F1", "ROC"))

for(i in 1:length(train_index)){
  performance[i] <- function_modeling(fold = i, "svmRadial")
}
performance$avg <- rowMeans(performance)
# test_data_w_target <- cbind(test_data, pred)
