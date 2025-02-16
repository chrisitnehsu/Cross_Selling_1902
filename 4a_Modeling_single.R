model_choose <- "nnet"
set.seed(12)
train_index <- createFolds(all_data_valid$target_var, k = 4, returnTrain = T)


function_modeling <- function(model_choose){
set.seed(12)
  
tt <- function_engineering(train_data)
processed_test_data <- function_engineering(test_data)

model <- train(
  target_var ~ ., 
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
conf <- confusionMatrix(pred, processed_test_data$target_var, mode = "everything",
                        positive = "success");conf

#AUC
pred <- predict(model, processed_test_data,  type = "prob")
AUC <- colAUC(pred, processed_test_data[["target_var"]])
AUC
#
metrics <- as.vector(cbind(conf$overall[["Accuracy"]]
                           ,conf$byClass[["Sensitivity"]]
                           ,conf$byClass[["Precision"]]
                           ,conf$byClass[["F1"]]
                           ,AUC[[1]])) %>% as.data.frame
rownames(metrics) <- c("Accuracy", "Sensitivity", "Precision", "F1", "ROC")
# names(metrics)[1] <- substitute(fold)
return(metrics)}

#合併模型結果
performance <- data.frame(V1 = c(1,1,1,1,1), row.names = c("Accuracy", "Sensitivity", "Precision", "F1", "ROC"))

for(i in 1:length(train_index)){
  train_data <- all_data_valid[train_index[[i]],]
  test_data <- all_data_valid[-train_index[[i]],]

  performance[i] <- function_modeling(model_choose)
}
performance$avg <- rowMeans(performance)
performance %>% View



#把預測結果和實際結果放一起看看,測試分級
# test_data_w_target <- data.frame(pred$success, test_data)
# test_data_w_target %>% group_by(target_var) %>% summarise(avg_pred = mean(pred.success))
# test_data_w_target$cut <- cut(test_data_w_target$pred.success, c(0,0.33,0.66,1))
# t1 <- test_data_w_target %>% group_by(cut) %>% filter(target_var == "success") %>% summarise(rate = n())
# t2 <- test_data_w_target %>% group_by(cut) %>% summarise(rate = n())
# t1$rate/t2$rate
