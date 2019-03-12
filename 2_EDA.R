# source("0_Source.R", encoding = "utf8")

#---------------------------------target variable description
#target variable:if_ticket_success, if_book_success, if_other_success, if_any_success
#succsess rate解釋:2018/08/01至今有被推銷過某品類的人,至少有一通成交該品類的電話的機率
sum(all_data$if_ticket_success == "success", na.rm = T)/sum(!is.na(all_data$if_ticket_success))
#ticket 334, 12%
#book 1281, 23% 
#other 213, 8%
#all 1708, 21%


#filter ticket data
all_data_ticket <- filter(all_data, !is.na(if_ticket_success))


#---------------------------------EDA---------------------------------#
summary(all_data_ticket$if_ticket_success)

#NA ratio: CHILDREN:62.2%, EDUCATION: 59.4%, GRADE: 52.0%, Positions 51.5%, MARRIED 45.5%, Age 32.7%,
#OCCUPATIONAL: 28.2%, industry_category: 17.1%, GENDER: 10.5%
function_unitable <- function(x, countNA = F){
  if(countNA == F) {all_data_ticket <- filter(all_data_ticket, !is.na(!!x))}else{}
  all_data_ticket %>% group_by(!!x) %>% 
    count %>% mutate(pct = 100 * n / nrow(all_data_ticket)) %>% arrange(desc(n)) %>% as.data.frame()
}

sapply(all_data_ticket,function(x){sum(is.na(x)) / nrow(all_data_ticket)}) %>% sort(decreasing = T) 

categorical_var <- sapply(all_data_ticket[-c(1:6)], class) %>% .[. == "factor"] %>% names
numerical_var <- sapply(all_data_ticket[-c(1:6)], class) %>% .[. != "factor"] %>% names

#---------------------------------categorical var.
#count table, try loop?
function_unitable(quo(AREA_NO))


#notes: levels issues
#1.Order_State停寄應該合併至斷訂
#2.AREA_NO可能配合CITY改成雙北,其他北部,中,南,東部與離島
#3.MAIL_TO 國外設為NA?這種獨立又少的該怎麼辦?
#4.CHANNEL_NAME不可能用, CHANNEL_CATEGORY要研究怎麼再組
#5.industry_category也要合併
#6.sales_person怎處理?
#7.Positions自雇工作者


#cross count table
function_crosstable <- function(x){
assocstats(table(data$if_ticket_success, x))$table %>% print()
prop.table(table(data$if_ticket_success, x), margin = 2) %>% print()
assocstats(table(data$if_ticket_success, x)) %>% print()
}

function_crosstable(data$if_other_success)


#notes: features issues
#O:GENDER(女好)),MAIL_TO(公司好!),IsGrandRealEstate(沒有的好!),IsGrandCreditCard(有好)),IF_USE_TAX(有好),
#X:MARRIED,AREA_NO,identity,Positions
#待修正:CITY,Order_State系列,CHANNEL_CATEGORY

#其他商品跨售成功影響很大,但不放進模型

#修正後
# O:industry_category從沒有變有,HAS_ONLINE_ORDER(沒有的好),Order_State_Bw_PE_Mg(沒有變有)
# X:Positions,Latest_Mag_Bundle,AREA_NO,Order_State_ST_PE_Mg(ST全系列),Order_State_EMGBW,Order_State_GOLF

#零變異屬量轉類別(成功的樣本數太少者就不放)
#O:Total_Order_Amt_Life,ITEM_COUNT_ARTBOOK(nearly),TICKET_ORDER_BEFORE

#categorical data viz
ggplot(filter(all_data_ticket, !is.na(if_ticket_success)), aes(x = GENDER, fill = if_ticket_success))+
  geom_bar(position = "fill")

#---------------------------------numerical var.

#univariable analysis
summary(all_data_ticket$Latest_Order_From_180801)

#rate of greater than 0
1-(sum(all_data_ticket$PR == 0) / nrow(all_data_ticket))
#跟課程活動有關的變數可能都要刪掉
#跨售過住宿券/書/其他約1%也考慮刪掉->合併後還是不到2%

ggplot(all_data_ticket, aes(x = 1, y = ITEM_COUNT_BIZBOOK))+
  geom_boxplot()+
  scale_y_continuous(breaks=seq(0, max(all_data_ticket$Total_Order_Amt_BWG), 100000))

ggplot(all_data_ticket, aes(x = Total_Order_Amt_BWG))+
  geom_histogram()+
  scale_x_continuous(breaks=seq(0, 100, 10))

#notes
#1.買過的書可能要改成0或1
#2.Latest_Order_From_180801可能要改成numerical
all_data_ticket$Latest_Order_From_180801 <- as.numeric(all_data_ticket$Latest_Order_From_180801)

#nearZeroVar
#數值變數為nearzero的, 轉換成0/1變數
#類別變數為nearzero的, 考慮拿掉, 除非具有明顯商業意義或是高相關
#拿掉:Order系列除了BW所有
nearZeroVars <- nearZeroVar(all_data_ticket, saveMetrics = TRUE) %>% .[.$nzv == TRUE,] %>% rownames_to_column()
nearZeroVars_numeric <- nearZeroVars$rowname[nearZeroVars$rowname %in% numerical_var]

index <- which(names(all_data_ticket) %in% nearZeroVars_numeric)
nearZero_to_01 <- all_data_ticket[,index]
for(i in 1:length(nearZero_to_01)){
  nearZero_to_01[i] <- ifelse(nearZero_to_01[i] >0, "大於0", "0") %>% as.factor()
}


nearZeroVars_numeric_drop <- (sapply(nearZero_to_01, function(x){sum(x == "大於0")}) / nrow(all_data_ticket)) %>% 
  .[. < 0.05] %>% names

#correleation analysis, check collinarity
findCorrelation(cor(all_data_ticket[names(all_data_ticket) %in% numerical_var &
  !names(all_data_ticket) %in% c("Age", "PR", "Latest_Order_From_180801","Latest_Mag_Units",'Order_Tenure_ST_PE_Mg','Order_Tenure_EMGBW','Order_Tenure_EMGST')]),  names = TRUE)
#建議刪掉Total_Order_Amt_BWG(w/Total_Order_Amt_BW), Act_Order_Amt_BWG(w/BW_Order_Amt_Stu)


#check Linear Combonations
findLinearCombos(cor(all_data_ticket[names(all_data_ticket) %in% numerical_var &
                                    !names(all_data_ticket) %in% c("Age", "PR", "Latest_Order_From_180801")]))
#無發現明顯共線性變數

#do T-Test to check relationship
t.test(data = all_data_ticket, 
       book_success_call
       ~ if_ticket_success, 
       var.equal=TRUE)

#X:Age, Act_Order_Amt_BWG, Total_Order_Amt_ST, 
#BW_Order_Amt_Adult, BW_Order_Count_Adult, BW_Order_Amt_Stu, BW_Order_Count_Stu,
#ST_Order_Amt_Adult, ST_Order_Count_Adult, Alive_Order_Amt_Adult(無值),
#Order_Tenure_ST_PE_Mg, BOOK_ORDER_BEFORE, PR, Latest_Order_From_180801
#Latest_Mag_Units新增,仍無


# BOOK_ORDER_BEFORE沒有 可以注意:套書跨售成功與否跟住宿券無關
# Latest_Order_From_180801沒有 意外

