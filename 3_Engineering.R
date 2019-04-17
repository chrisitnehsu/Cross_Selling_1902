#filter ticket data
all_data_valid <- filter(all_data, !is.na(if_ticket_success))
# data <- all_data_valid

#---------------------------------Feature Engineering---------------------------------
function_engineering_pre <- function(data){
  #modify features based on EDA---------------------------------
  #1.Order_State停寄應該合併至斷訂
  data$Order_State_Bw_Mg <- factor(data$Order_State_Bw_Mg, 
                                   levels = c("現訂", "斷訂", "未訂", "停寄"),
                                   labels = c("現訂", "斷訂", "未訂", "斷訂"))
  data$Order_State_Bw_PE_Mg <- factor(data$Order_State_Bw_PE_Mg, 
                                      levels = c("現訂", "斷訂", "未訂", "停寄"),
                                      labels = c("現訂", "斷訂", "未訂", "斷訂"))
  data$Order_State_EMGBW <- factor(data$Order_State_EMGBW, 
                                   levels = c("現訂", "斷訂", "未訂", "停寄"),
                                   labels = c("現訂", "斷訂", "未訂", "斷訂"))
  data$Order_State_EMGST <- factor(data$Order_State_EMGST, 
                                   levels = c("現訂", "斷訂", "未訂", "停寄"),
                                   labels = c("現訂", "斷訂", "未訂", "斷訂"))
  data$Order_State_GOLF <- factor(data$Order_State_GOLF, 
                                  levels = c("現訂", "斷訂", "未訂", "停寄"),
                                  labels = c("曾訂", "曾訂", "未訂", "曾訂"))
  data$Order_State_ST_PE_Mg <- factor(data$Order_State_ST_PE_Mg, 
                                      levels = c("現訂", "斷訂", "未訂", "停寄"),
                                      labels = c("現訂", "斷訂", "未訂", "斷訂"))
  data$Order_State_ST_Mg <- factor(data$Order_State_ST_Mg, 
                                   levels = c("現訂", "斷訂", "未訂", "停寄"),
                                   labels = c("現訂", "斷訂", "未訂", "斷訂"))
  
  #2.CITY可能改成雙北,其他北部,中,南,東部與離島
  data$AREA_NO <- as.character(data$AREA_NO)
  data$AREA_NO[data$AREA_NO == "北部" & 
                 data$CITY %in% c("台北市","新北市")] <- "雙北"
  data$AREA_NO[data$AREA_NO == "北部" & 
                 !data$CITY %in% c("台北市","新北市")] <- "其他北部"
  data$AREA_NO[data$AREA_NO == "東部" | 
                 data$AREA_NO == "離島"] <- "東部或離島"
  data$AREA_NO <- factor(data$AREA_NO)
  
  
  #3.MAIL_TO 國外設為NA?這種獨立又少的該怎麼辦?->因為清楚意涵,所以更不能把他合併在一起
  
  #4.CHANNEL_NAME不可能用, CHANNEL_CATEGORY要研究怎麼再組
  #決定應加入案型與網路下單欄位
  
  #5.industry_category也要合併
  data$industry_category <- factor(data$industry_category, 
                                   levels = c("批發零售","製造業","金融業","一般性服務業","其他行業","資訊科技","專業性服務業","營建業","軍公教","不動產業","保險業","媒體傳播","運輸倉儲業","退休","餐飲業","學生","農林漁牧礦業","家管","住宿業","能源業","醫療業","貿易業"),
                                   labels = c("批發零售","製造業","金融業","服務業","其他行業","資訊科技","服務業","營建業","軍公教","不動產業","保險業","其他行業","其他行業","退休或家管","餐飲住宿業","其他行業","其他行業","退休或家管","餐飲住宿業","其他行業","其他行業","其他行業"))
  
  #6.sales_person怎處理?->可能先不要放進去
  #7.Positions自雇工作者->覺得自雇工作者=自由業=職員
  data$Positions <- factor(data$Positions,
                           levels = c("企業負責人","高階主管","中階及基層主管","職員","自僱工作者"),
                           labels = c("企業負責人","高階主管","中階及基層主管","職員","職員"))
  
  data$MAIL_TO <- factor(data$MAIL_TO,
                         levels = c("住家","國外","公司"),
                         labels = c("住家","公司或國外","公司或國外"))
  
  
  data$HAS_ONLINE_ORDER <- factor(data$HAS_ONLINE_ORDER)
  
  
  #把接近零變異的連續變數轉為0/1類別變數
  numerical_var <- sapply(data[-c(1:6)], class) %>% .[. != "factor"] %>% names
  
  nearZeroVars <- nearZeroVar(data, saveMetrics = TRUE) %>% .[.$nzv == TRUE,] %>% rownames_to_column()
  nearZeroVars_numeric <- nearZeroVars$rowname[nearZeroVars$rowname %in% numerical_var]
  
  index <- which(names(data) %in% nearZeroVars_numeric)
  nearZero_to_01 <- data[,index]
  for(i in 1:length(nearZero_to_01)){
    nearZero_to_01[i] <- ifelse(nearZero_to_01[i] >0, "大於0", "0") %>% as.factor()
  }
  
  
  nearZeroVars_numeric_drop <- (sapply(nearZero_to_01, function(x){sum(x == "大於0")}) / nrow(all_data_valid)) %>% 
    .[. < 0.05] %>% names
  
  
  #NA% > 40%, but keep Positions
  NA_drop <-  sapply(all_data_valid,function(x){sum(is.na(x)) / nrow(all_data_valid)}) %>% sort(decreasing = T) %>% 
    .[.>0.4 & names(.) != "Positions"] %>% names
  
  #useless features
  useless_features <- c("CITY", "OCCUPATIONAL", "GRADE", "CHANNEL_NAME", "CHANNEL_CATEGORY")
  #correlation features
  high_co_features <- c("Total_Order_Amt_BWG", "Act_Order_Amt_BWG")
  
  #delete 1.known useless features 2.zero variance features & NA% greater than 40%(may cause model unstable)
  data <- select(data, -c(Cust_ID, if_book_success:other_unsure_call))
  data <- data[!names(data) %in% useless_features]
  data <- data[!names(data) %in% nearZeroVars_numeric_drop]
  data <- data[!names(data) %in% NA_drop]
  data <- data[!names(data) %in% high_co_features]
  
  return(data)
}  

all_data_valid <- function_engineering_pre(all_data_valid)


# impute missing values
set.seed(12)   
target_var <- all_data_valid[c("if_ticket_success")] 
mice_model <- mice(all_data_valid[-1], m=1, maxit = 5, seed = 50)
all_data_valid <- cbind(target_var, complete(mice_model,1))


#Feature selection---------------------------------
#rf variable importance filter, mtry use caret's best tune
set.seed(12)   
# rf_selection_model_test <- train(
#   if_ticket_success ~ .,
#   data = all_data_valid,
#   method = "rf",
#   metric = "Sens",
#   trControl = trainControl(
#     method = "cv", number = 4,
#     verboseIter = TRUE,
#     summaryFunction = twoClassSummary,
#     classProbs = TRUE
#   )
# )


rf_selection_model <- randomForest(if_ticket_success~., data = all_data_valid, mtry = 2)
importance <- randomForest::importance(rf_selection_model) %>% 
  as.data.frame() %>% rownames_to_column() %>% arrange(desc(MeanDecreaseGini))
features_in <- importance[c(1:20),1]

all_data_valid <- all_data_valid[names(all_data_valid) %in% features_in] 
all_data_valid <- cbind(target_var, all_data_valid)




#resampling & encoding & scaling
function_engineering <- function(data){ 
  
  #if test data, skip resampling phase
  if(nrow(data) == nrow(train_data)){
    
    data <- SMOTE(if_ticket_success~., data = data, perc.over = 300, perc.under =200)
    target_var_trainSMOTE <- data["if_ticket_success"] #resampling train data only bind label 
    #categorical data encoding. test: OHE or dummy?
    data <- dummyVars(if_ticket_success~.,data = data, fullRank = F) %>% 
      predict(newdata = data) %>% as.data.frame() %>% cbind(target_var_trainSMOTE)
    
    preProcess <- preProcess(data, method = c("center", "scale"))
    data <- predict(preProcess, data)
    
    
  }else{
    #categorical data encoding. test: OHE or dummy?
    target_var <- data["if_ticket_success"] 
    data <- dummyVars(if_ticket_success~.,data = data, fullRank = F) %>%
      predict(newdata = data) %>% as.data.frame() %>% cbind(target_var) #test data bind CID
    
    preProcess <- preProcess(data, method = c("center", "scale")) #不確定test的標準化參數要用自己的還是用train的
    data <- predict(preProcess, data)
    
  }
  return(data)
}