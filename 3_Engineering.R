#filter ticket data
all_data_ticket <- filter(all_data, !is.na(if_ticket_success))


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
  
  
  #把接近零變異的連續變數轉為0/1類別變數
  index <- which(names(data) %in% c('Act_Order_Amt_BWG','Total_Order_Amt_ST','Total_Order_Amt_Life','BW_Order_Amt_Adult','BW_Order_Count_Adult','BW_Order_Amt_Stu','BW_Order_Count_Stu','ST_Order_Amt_Adult','ST_Order_Count_Adult','Alive_Order_Amt_Adult','Alive_Order_Count_Adult','ITEM_COUNT_BIZBOOK','ITEM_COUNT_ARTBOOK','TICKET_ORDER_BEFORE','BOOK_ORDER_BEFORE','OTHER_ORDER_BEFORE'))
  
  for(i in index){
    data[i] <- ifelse(data[i] >0, "大於0", "0") %>% as.factor()
  }

  
  
  #Feature selection---------------------------------
  data <- select(data, -c('CITY','AREA_NO','EDUCATION','GRADE','OCCUPATIONAL','MARRIED','CHILDREN','Latest_Mag_Bundle','Act_Order_Amt_BWG','Total_Order_Amt_ST','BW_Order_Amt_Adult','BW_Order_Count_Adult','BW_Order_Amt_Stu','BW_Order_Count_Stu','ST_Order_Amt_Adult','ST_Order_Count_Adult','Alive_Order_Amt_Adult','Alive_Order_Count_Adult','ITEM_COUNT_BIZBOOK','BOOK_ORDER_BEFORE','OTHER_ORDER_BEFORE','Order_State_Bw_Mg','Order_State_ST_PE_Mg','Order_State_ST_Mg','Order_State_EMGBW','Order_State_EMGST','Order_State_GOLF','CHANNEL_NAME','CHANNEL_CATEGORY','identity','Positions'),
                 -c('total_call','ticket_success_call','ticket_faliure_call','ticket_unsure_call','book_success_call','book_faliure_call','book_unsure_call','other_success_call','other_faliure_call','other_unsure_call','Age','Latest_Mag_Units','Total_Order_Amt_BWG','Mag_Order_Amt_BWG','Order_Tenure_ST_PE_Mg','Order_Tenure_EMGBW','Order_Tenure_EMGST','PR','Latest_Order_From_180801'),
                 -c('if_book_success':'last_salesperson'))
 

target_var <- data[c("CustomerId","if_ticket_success")] 
  
  # impute missing values
  data <- mice(data[-c(1,2)], m=1, maxit = 5, seed = 50)
  data <- cbind(target_var,complete(data,1))
  
return(data)
}  
set.seed(12)   
all_data_ticket <- function_engineering_pre(all_data_ticket)


function_engineering <- function(data){ 
  target_var <- data[c("CustomerId","if_ticket_success")] 
  
  #if test data, skip resampling phase
  if(nrow(data) == nrow(train_data)){

    data <- SMOTE(if_ticket_success~., data = data[-1], perc.over = 300, perc.under = 200)
    target_var_trainSMOTE <- data["if_ticket_success"] #resampling train data only bind label 
    #categorical data encoding. test: OHE or dummy?
    data <- dummyVars(if_ticket_success~.,data = data, fullRank = F) %>% 
    predict(newdata = data) %>% as.data.frame() %>% cbind(target_var_trainSMOTE)
  
  }else{
    #categorical data encoding. test: OHE or dummy?
    data <- dummyVars(if_ticket_success~.,data = data[-1], fullRank = F) %>%
    predict(newdata = data) %>% as.data.frame() %>% cbind(target_var[c(1,2)]) #test data bind CID
  }
  return(data)
}

