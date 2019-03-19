set.seed(12)
model <- train(
  if_ticket_success ~ ., 
  data = modeling_data,
  method = model[1],
  metric = "Sens",
  trControl = trainControl(
    method = "cv", number = 4,
    verboseIter = TRUE,
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    savePredictions = "final"))

# 
# resample_stats <- thresholder(mod, 
#                               threshold = seq(.5, 1, by = 0.05), 
#                               final = TRUE)

pred <- predict(model, processed_test_data,  type = "raw")
conf <- confusionMatrix(pred, processed_test_data$if_ticket_success, mode = "everything",
                        positive = "success")

pred <- predict(model, processed_test_data,  type = "prob")
AUC <- colAUC(pred, processed_test_data[["if_ticket_success"]])


test_data_w_target <- cbind(test_data, pred)
