library(RODBC)
library(dplyr)
library(stringr)
library(lubridate)
library(openxlsx)
library(readxl)
library(VIM)
library(mice)
library(rebus)
library(ggplot2)
library(caret)
library(vcd)
library(DMwR)
library(mice)
library(caTools)
library(kknn)
library(RANN)
library(tibble)
library(randomForest)
library(Boruta)
library(caretEnsemble)

options(scipen=999)

#sales related data only update to 2018/8/01 for modeling
load("CRMDB_CROSS_SELLING_190327.Rdata")

conn_DW <- odbcConnect("BWG_DW_UAT", uid="80380", pwd="q6eXAYqB")

raw_data <- sqlQuery(conn_DW, "SELECT *, case when Product_Name in ('世界料理解購聖經','世界最美鐵道','世界遺產全書','地中海史詩套書','套書-Smart出版套書','套書-公孫策說歷史故事','套書-世界料理解購聖經','套書-世界最美鐵道','套書-世界遺產全書','套書-地中海史詩套書','套書-男女健身套書','套書-從黎明到衰頹','套書-掌握世界品味套書','套書-葡萄酒三書','掌握世界品味套書','葡萄酒三書','套書-改變世界葡萄酒') then '套書'
			   when Product_Name in ('台北亞都麗緻飯店','尖山埤江南渡假村','竹湖麗緻','住宿券-大板根森林溫泉渡假村','住宿券-台中大毅老爺行旅','住宿券-台中長榮桂冠','住宿券-台北西華飯店','住宿券-台北沃田旅店','住宿券-台北亞都麗緻飯店','住宿券-台東GAYA酒店','住宿券-台南遠東大飯店','住宿券-宜蘭傳藝老爺行旅','住宿券-宜蘭綠舞','住宿券-高雄晶英行旅','住宿券-高雄漢來飯店','住宿券-理想大地','住宿券-陽明山麗緻','住宿券-新竹THE ONE南園','住宿券-煙波飯店','住宿券-嘉義觀止','住宿券-馥蘭朵','妖怪村','其他-陽明山麗緻泡湯','宜蘭村卻','東允餐券','煙波飯店','嘉義觀止','嘉儀電器','綠舞觀光飯店','餐券-高雄雅樂廚苑','住宿券-台北萬豪酒店','住宿券-台北美福大飯店','住宿券-台南桂田酒店','住宿券-墾丁嵐翊白紗渡假莊','住宿券-台南晶英酒店','住宿券-宜蘭晶泉丰旅','住宿券-太魯閣晶英酒店','住宿券-台北晶華酒店') then '住宿券或餐券'
         when Product_Name in ('商業周刊', '智富月刊', '課程', '商周電子', '智富電子') or Product_Name LIKE '雜誌%' then '雜誌'   
         when Product_Name in ('圓桌-圓桌論壇') then '圓桌論壇'   
         else '其他' end as ProductCategory,
             case when Inclination_Name = '成交' then '成交'
             when Inclination_Name like '未成交%' then '未成交'
             else '未確定' end as State
             FROM [BWG_DW_UAT].[dbo].[DW_Outbound]
             where Product_Name is not null 
             and Group_Name = '訂戶維運部'
             and Created_Date >= '2018-08-01'
             ", as.is = T)
#sales資料也要用分析期之前的最後一筆,才符合情境
sales_person_data <- sqlQuery(conn_DW, "SELECT Cust_ID, SA_NAME, Created_Date
                              from [BWG_DW_UAT].[dbo].[DW_Outbound]
                              where Group_Name = '訂戶維運部'and Created_Date < '2018-08-01'",as.is = T)

last_salesperson <- sales_person_data %>% arrange(Cust_ID, desc(Created_Date)) %>% group_by(Cust_ID) %>% 
  summarise(last_salesperson = first(SA_NAME))

last_salesperson$last_salesperson[last_salesperson$last_salesperson == "張桂珠_old"] <- "張桂珠"
last_salesperson$last_salesperson[last_salesperson$last_salesperson == "黃琪_old"] <- "黃琪"

raw_data$Created_Date <- ymd_hms(raw_data$Created_Date)

raw_data$from_now <- difftime(today(), raw_data$Created_Date, units = "days")


contact_count <- raw_data %>% group_by(Cust_ID) %>% summarise(total_call = n())
ticket_success <- raw_data %>% filter(ProductCategory == "住宿券或餐券", State == "成交") %>% group_by(Cust_ID) %>% summarise(ticket_success_call = n())
book_success <- raw_data %>% filter(ProductCategory == "套書", State == "成交") %>% group_by(Cust_ID) %>% summarise(book_success_call = n())
mag_success <- raw_data %>% filter(ProductCategory == "雜誌", State == "成交") %>% group_by(Cust_ID) %>% summarise(mag_success_call = n())
forum_success <- raw_data %>% filter(ProductCategory == "圓桌論壇", State == "成交") %>% group_by(Cust_ID) %>% summarise(forum_success_call = n())
other_success <- raw_data %>% filter(ProductCategory == "其他", State == "成交") %>% group_by(Cust_ID) %>% summarise(other_success_call = n())

#faliure means not deal or unsure for more than 60 days
ticket_faliure <- raw_data %>% filter(ProductCategory == "住宿券或餐券", 
                                      (State == "未成交"| (State == "未確定" & from_now >=60))) %>% group_by(Cust_ID) %>% summarise(ticket_faliure_call = n())
book_faliure <- raw_data %>% filter(ProductCategory == "套書", 
                                    (State == "未成交"| (State == "未確定" & from_now >=60))) %>% group_by(Cust_ID) %>% summarise(book_faliure_call = n())
mag_faliure <- raw_data %>% filter(ProductCategory == "雜誌", 
                                   (State == "未成交"| (State == "未確定" & from_now >=60))) %>% group_by(Cust_ID) %>% summarise(mag_faliure_call = n())
forum_faliure <- raw_data %>% filter(ProductCategory == "圓桌論壇", 
                                     (State == "未成交"| (State == "未確定" & from_now >=60))) %>% group_by(Cust_ID) %>% summarise(forum_faliure_call = n())
other_faliure <- raw_data %>% filter(ProductCategory == "其他", 
                                     (State == "未成交"| (State == "未確定" & from_now >=60))) %>% group_by(Cust_ID) %>% summarise(other_faliure_call = n())


ticket_uncertain <- raw_data %>% filter(ProductCategory == "住宿券或餐券", State == "未確定", from_now < 60) %>% group_by(Cust_ID) %>% summarise(ticket_unsure_call = n())
book_uncertain <- raw_data %>% filter(ProductCategory == "套書", State == "未確定", from_now < 60) %>% group_by(Cust_ID) %>% summarise(book_unsure_call = n())
mag_uncertain <- raw_data %>% filter(ProductCategory == "雜誌", State == "未確定", from_now < 60) %>% group_by(Cust_ID) %>% summarise(mag_unsure_call = n())
forum_uncertain <- raw_data %>% filter(ProductCategory == "圓桌論壇", State == "未確定", from_now < 60) %>% group_by(Cust_ID) %>% summarise(forum_unsure_call = n())
other_uncertain <- raw_data %>% filter(ProductCategory == "其他", State == "未確定", from_now < 60) %>% group_by(Cust_ID) %>% summarise(other_unsure_call = n())




all_data <- distinct(raw_data["Cust_ID"]) %>% arrange(Cust_ID) %>% 
  left_join(last_salesperson) %>% 
  left_join(contact_count) %>% 
  left_join(ticket_success) %>% 
  left_join(ticket_faliure) %>% 
  left_join(ticket_uncertain) %>% 
  left_join(book_success) %>% 
  left_join(book_faliure) %>% 
  left_join(book_uncertain) %>% 
  left_join(mag_success) %>% 
  left_join(mag_faliure) %>% 
  left_join(mag_uncertain) %>% 
  left_join(forum_success) %>% 
  left_join(forum_faliure) %>% 
  left_join(forum_uncertain) %>% 
  left_join(other_success) %>% 
  left_join(other_faliure) %>% 
  left_join(other_uncertain) 

all_data[] <- lapply(all_data, function(x){ifelse(!is.character(x)&is.na(x),0,x)})
all_data$last_salesperson <- factor(all_data$last_salesperson)



#create label
#電銷接觸過任一產品,且至少結過一次成功或失敗,即進入data set

all_data$if_ticket_success[all_data$ticket_success_call + 
                             all_data$ticket_faliure_call +
                             all_data$book_success_call +
                             all_data$book_faliure_call +
                             all_data$mag_success_call +
                             all_data$mag_faliure_call +
                             all_data$forum_success_call +
                             all_data$forum_faliure_call +
                             all_data$other_success_call +
                             all_data$other_faliure_call >0] <- "faliure"

all_data$if_ticket_success[all_data$ticket_success_call >0] <- "success"

all_data$if_book_success[all_data$ticket_success_call + 
                           all_data$ticket_faliure_call +
                           all_data$book_success_call +
                           all_data$book_faliure_call +
                           all_data$mag_success_call +
                           all_data$mag_faliure_call +
                           all_data$forum_success_call +
                           all_data$forum_faliure_call +
                           all_data$other_success_call +
                           all_data$other_faliure_call >0] <- "faliure"
all_data$if_book_success[all_data$book_success_call >0] <- "success"

all_data$if_other_success[all_data$ticket_success_call + 
                            all_data$ticket_faliure_call +
                            all_data$book_success_call +
                            all_data$book_faliure_call +
                            all_data$mag_success_call +
                            all_data$mag_faliure_call +
                            all_data$forum_success_call +
                            all_data$forum_faliure_call +
                            all_data$other_success_call +
                            all_data$other_faliure_call >0] <- "faliure"
all_data$if_other_success[all_data$other_success_call >0] <- "success"

#任一商品有結過成交或是未成交都算
all_data$if_any_success[all_data$ticket_success_call + 
                            all_data$ticket_faliure_call +
                            all_data$book_success_call +
                            all_data$book_faliure_call +
                            all_data$mag_success_call +
                            all_data$mag_faliure_call +
                            all_data$forum_success_call +
                            all_data$forum_faliure_call +
                            all_data$other_success_call +
                            all_data$other_faliure_call >0] <- "faliure"

all_data$if_any_success[all_data$ticket_success_call+
                        all_data$book_success_call+ 
                        all_data$other_success_call > 0] <- "success"


all_data$if_ticket_success <- factor(all_data$if_ticket_success, levels = c("success", "faliure"), labels = c("success", "faliure"))
all_data$if_book_success <- factor(all_data$if_book_success, levels = c("success", "faliure"), labels = c("success", "faliure"))
all_data$if_other_success <- factor(all_data$if_other_success, levels = c("success", "faliure"), labels = c("success", "faliure"))
all_data$if_any_success <- factor(all_data$if_any_success, levels = c("success", "faliure"), labels = c("success", "faliure"))


#join CRM
#維持只選擇上一個版本的初始欄位
all_data <- inner_join(all_data, CRMDB, c("Cust_ID" = "CUST_ID"))
all_data <- select(all_data, c("Cust_ID","if_ticket_success","if_book_success","if_other_success","if_any_success",'last_salesperson','total_call','ticket_success_call','ticket_faliure_call','ticket_unsure_call','book_success_call','book_faliure_call','book_unsure_call','other_success_call','other_faliure_call','other_unsure_call','CITY','AREA_NO','EDUCATION','GRADE','OCCUPATIONAL','Age','MARRIED','CHILDREN','GENDER','MAIL_TO','IsGrandRealEstate','IsGrandCreditCard','HAS_ONLINE_ORDER','Latest_Mag_Bundle','Latest_Mag_Units','Total_Order_Amt_BWG','Mag_Order_Amt_BWG','Act_Order_Amt_BWG','Others_Order_Amt_BWG','Total_Order_Amt_BW','Total_Order_Amt_ST','Total_Order_Amt_Life','BW_Order_Amt_Adult','BW_Order_Count_Adult','BW_Order_Amt_Stu','BW_Order_Count_Stu','ST_Order_Amt_Adult','ST_Order_Count_Adult','Alive_Order_Amt_Adult','Alive_Order_Count_Adult',"ITEM_COUNT_BIZBOOK","ITEM_COUNT_ARTBOOK","TICKET_ORDER_BEFORE","BOOK_ORDER_BEFORE","OTHER_ORDER_BEFORE",'Order_State_Bw_PE_Mg','Order_Tenure_Bw_PE_Mg','Order_State_Bw_Mg','Order_State_ST_PE_Mg','Order_Tenure_ST_PE_Mg','Order_State_ST_Mg','Order_State_EMGBW','Order_Tenure_EMGBW','Order_State_EMGST','Order_Tenure_EMGST','Order_State_GOLF','PR','CHANNEL_NAME','CHANNEL_CATEGORY','Latest_Order_From_180801','IF_USE_TAX','identity','Positions','industry_category'))

all_data$Order_Tenure_Bw_PE_Mg[is.na(all_data$Order_Tenure_Bw_PE_Mg)] <- 0
all_data$Order_Tenure_EMGBW[is.na(all_data$Order_Tenure_EMGBW)] <- 0
all_data$Order_Tenure_EMGST[is.na(all_data$Order_Tenure_EMGST)] <- 0
all_data$Order_Tenure_ST_PE_Mg[is.na(all_data$Order_Tenure_ST_PE_Mg)] <- 0

#有幾格是NA的
all_data$count_NA <- apply(all_data, 1,function(x){sum(is.na(x))})

# rm(list = ls()[!ls() %in% c("raw_data","all_data")])
