model <- c("kknn", "naive_bayes", "svmRadial", "rf", "xgbTree", "nnet", "gbm")

#create fold
set.seed(12)
train_index <- createFolds(all_data_ticket$if_ticket_success, k = 4, returnTrain = T)


#training models
function_train_evaluation <- function(model){
  
  preProcess <- preProcess(modeling_data, method = c("center", "scale"))
  modeling_data <- predict(preProcess, modeling_data)
  preProcess <- preProcess(processed_test_data, method = c("center", "scale")) #不確定test的標準化參數要用自己的還是用train的
  processed_test_data <- predict(preProcess, processed_test_data)
  
  model <- train(
    if_ticket_success ~ ., 
    data = modeling_data,
    method = model,
    metric = "Sens",
    trControl = trainControl(
      method = "cv", number = 4,
      verboseIter = TRUE,
      summaryFunction = twoClassSummary,
      classProbs = TRUE
    )
  )
  
  
  processed_test_data <- select(processed_test_data, -CustomerId)
  
  pred <- predict(model, processed_test_data,  type = "raw")
  conf <- confusionMatrix(pred, processed_test_data$if_ticket_success, mode = "everything",
                          positive = "success")
  
  pred <- predict(model, processed_test_data,  type = "prob")
  AUC <- colAUC(pred, processed_test_data[["if_ticket_success"]])
  
  
  metrics <- as.vector(cbind(conf$overall[["Accuracy"]]
                             ,conf$byClass[["Sensitivity"]]
                             ,conf$byClass[["Precision"]]
                             ,conf$byClass[["F1"]]
                             ,AUC[[1]])) %>% as.data.frame
  
  rownames(metrics) <- c("Accuracy", "Sensitivity", "Precision", "F1", "ROC")
  names(metrics)[1] <- as.character(substitute(model))
  
  return(list(model = model, conf = conf, metrics = metrics))
}


#bulid model and loop outer CV
set.seed(12)
metrics_compare_all <- list()
model_information_all <- list()

for(i in 1:length(train_index)){
  test_data <- all_data_ticket[-train_index[[i]],]
  train_data <- all_data_ticket[train_index[[i]],]
  
  modeling_data <- function_engineering(train_data)
  processed_test_data <- function_engineering(test_data)
  
  metrics_compare <- data.frame(V1 = c(1,1,1,1,1), row.names = c("Accuracy", "Sensitivity", "Precision", "F1", "ROC"))
  model_information <- list()
  
      for(j in 1:length(model)){ #warning:length must be >=2, otherwise it will show error
        new_model_inf <- function_train_evaluation(model = model[j])
        metrics_compare <- cbind(metrics_compare, new_model_inf$metrics)
        model_information <- c(model_information, new_model_inf)
        rm(new_model_inf)
      }
  
  metrics_compare <- t(metrics_compare) %>% .[-1,] %>% as.data.frame()  %>% rownames_to_column() 
  metrics_compare$Accu_threshold <- ifelse(metrics_compare$Accuracy >= 0.6, "PASS", "X")
  
  metrics_compare_all[[i]] <- metrics_compare
  model_information_all[[i]] <- model_information
  
  rm(metrics_compare)
  rm(model_information)
}

metrics_compare_all <- metrics_compare_all %>% do.call(rbind,.)
metrics_compare_all$fold <- rep(1:length(train_index), each = length(model)) 

metrics_integration <- metrics_compare_all %>% group_by(rowname) %>% 
                       summarise(Accuracy_mean = mean(Accuracy, na.rm = T),
                                 Sensitivity_mean = mean(Sensitivity, na.rm = T),
                                 Precision_mean = mean(Precision, na.rm = T),
                                 F1_mean = mean(F1, na.rm = T),
                                 ROC_mean = mean(ROC, na.rm = T),
                                 Accuracy_sd = sd(Accuracy, na.rm = T),
                                 Sensitivity_sd = sd(Sensitivity, na.rm = T),
                                 Precision_sd = sd(Precision, na.rm = T),
                                 F1_sd = sd(F1, na.rm = T),
                                 ROC_sd = sd(ROC, na.rm = T)) %>% as.data.frame() %>% arrange(desc(F1_mean))
metrics_integration$Accu_threshold <- ifelse(metrics_integration$Accuracy_mean >= 0.6, "PASS", "X")


write.xlsx(metrics_integration, "tt.xlsx")
