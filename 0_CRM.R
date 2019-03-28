library(RODBC)
library(dplyr)
library(stringr)
library(lubridate)
library(rebus)
options(scipen=999)

#時間限制在2018-08-01之前,為了建模用

data <- function(x){
  connB2C <- odbcConnect("B2CDB", uid="80380", pwd="q6eXAYqB")
  
  SOT <-  sqlQuery(connB2C, "WITH SBK AS (
                   SELECT a.SOT_NO
                   ,a.SOT_SID
                   ,DOC_TYPE
                   FROM [172.24.203.41].[B2CDB01].[dbo].[BC_SBK_DETAIL] a
                   LEFT OUTER JOIN [172.24.203.41].[B2CDB01].[dbo].[BC_SBK_MASTER] b ON a.SBK_NO=b.SBK_NO AND b.DELETE_MARK='N'
                   WHERE a.DELETE_MARK='N' AND DOC_TYPE='2'),
                   UDC_LIST AS (
                   SELECT [CODE_ID]
                   ,[CODE_NAME]
                   FROM [B2CDB01].[dbo].[BC_UDC_LIST]
                   where SYSTEM_CODE = 50 and UDC_CODE = 'C1'),
                   
                   CHANNEL_MAP AS (
                   SELECT [CHANNEL_ID] 
                   ,[NAME] CHANNEL_NAME
                   ,[CODE_NAME] CHANNEL_CATEGORY
                   FROM [B2CDB01].[dbo].[BC_CHANNEL_MASTER] a left join UDC_LIST on a.CHANNEL_ID1 = UDC_LIST.CODE_ID),
                   
                   BUNDLE AS (SELECT  [PROD_ID],
                   (CASE WHEN CLASSID3 = '01' THEN '贈期'
                   WHEN CLASSID3 IN ('02','05') THEN '贈品'
                   WHEN CLASSID3 = '03' THEN '聯購'
                   ELSE '無贈' END) AS BUNDLE
                   FROM [B2CDB01].[dbo].[BC_STOCK_MASTER]
                   where CLASSID1 = '01')

                   SELECT a.SOT_NO,a.SOT_SID,a.CUST_ID,a.PROD_ID,b.TYPE_ID,b.KIND_ID,b.COST_CENTER,b.CAMP_TARGET,a.SOT_AMT,a.SBK_AMT,a.QTY,convert(char(10), a.SOT_DATE, 120) AS SOT_DATE,a.CHANNEL_ID,CHANNEL_NAME,CHANNEL_CATEGORY,a.GUI_NO,b.UNIT_VOLS,b.CLASSID1,b.CLASSID2,b.CLASSID3,b.CLASSID4,BUNDLE.BUNDLE
                   FROM [B2CDB01].[dbo].[BC_SOT_DETAIL] a
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_STOCK_MASTER] b ON a.PROD_ID=b.PROD_ID AND b.DELETE_MARK='N'
                   LEFT OUTER JOIN SBK ON a.SOT_NO=SBK.SOT_NO AND a.SOT_SID=SBK.SOT_SID
                   LEFT OUTER JOIN CHANNEL_MAP ON a.CHANNEL_ID = CHANNEL_MAP.CHANNEL_ID
                   LEFT OUTER JOIN BUNDLE ON a.PROD_ID = BUNDLE.PROD_ID  
                   WHERE a.DELETE_MARK='N' AND a.STATUS_ID NOT IN ('6','7') AND SBK.DOC_TYPE IS NULL 
                   AND a.SOT_DATE < '2018-08-01'
                   ORDER BY a.CUST_ID,a.SOT_DATE desc",stringsAsFactors=FALSE)
  
  SOT$GUI_NO <- gsub(" ",replacement = "",SOT$GUI_NO)
  Last_tax_ID <- SOT[duplicated(SOT$CUST_ID)==0,] %>% .[,c(3,16)]
  names(Last_tax_ID) <- c("CUST_ID","Last_tax_ID")
  
  Newest_tax_ID <- SOT[SOT$GUI_NO!="" & !is.na(SOT$GUI_NO) ,] %>% .[,c("CUST_ID","GUI_NO")] %>% .[duplicated(.$CUST_ID)==0,]
  names(Newest_tax_ID) <- c("CUST_ID","Newest_tax_ID")
  
  rm <- openxlsx::read.xlsx("V:/1.0_CRM/07.Regular Update Data/直銷商名單.xlsx")
  rm_gui <- rm$company_uid
  
  Newest_tax_ID_true <- SOT[SOT$GUI_NO!="" & !is.na(SOT$GUI_NO) & !(SOT$GUI_NO %in% rm_gui),] %>% .[,c("CUST_ID","GUI_NO")] %>% .[duplicated(.$CUST_ID)==0,]
  names(Newest_tax_ID_true) <- c("CUST_ID","Newest_tax_ID_true")
  
  Latest_Order_Date_BWG <- SOT[duplicated(SOT$CUST_ID)==0,] %>% .[,c("CUST_ID","SOT_DATE")]
  names(Latest_Order_Date_BWG) <- c("CUST_ID","Latest_Order_Date_BWG")
  
  Latest_Order_Channel_BWG <- SOT[duplicated(SOT$CUST_ID)==0,] %>% .[,c("CUST_ID","CHANNEL_ID", "CHANNEL_NAME", "CHANNEL_CATEGORY")]
  names(Latest_Order_Channel_BWG) <- c("CUST_ID","Latest_Order_Channel_BWG", "CHANNEL_NAME", "CHANNEL_CATEGORY")
  
  Latest_Mag_Bundle <- SOT[SOT$CLASSID1 == "1",] %>% .[duplicated(.$CUST_ID)==0,] %>% .[,c("CUST_ID","BUNDLE")]
  names(Latest_Mag_Bundle) <- c("CUST_ID", "Latest_Mag_Bundle")
  
  Latest_Mag_Units <- SOT[SOT$CLASSID1 == "1",] %>% .[duplicated(.$CUST_ID)==0,] %>% .[,c("CUST_ID","UNIT_VOLS")]
  names(Latest_Mag_Units) <- c("CUST_ID", "Latest_Mag_Units")
  
  CRM <-  sqlQuery(connB2C, "WITH SBK AS (
                   SELECT a.SOT_NO
                   ,a.SOT_SID
                   ,DOC_TYPE
                   FROM [172.24.203.41].[B2CDB01].[dbo].[BC_SBK_DETAIL] a
                   LEFT OUTER JOIN [172.24.203.41].[B2CDB01].[dbo].[BC_SBK_MASTER] b ON a.SBK_NO=b.SBK_NO AND b.DELETE_MARK='N'
                   WHERE a.DELETE_MARK='N' AND DOC_TYPE='2'),
                   
                   ONLINE_ORDER AS (SELECT DISTINCT ORDER_NO SOT_NO, 1 IF_ONLINE_ORDER
                   FROM [B2CDB01].[dbo].[BC_REC_AMTLIST] AS BRA
                   INNER JOIN [B2CDB01].[dbo].BC_REC_DETAIL AS BCD ON BCD.REC_NO = BRA.REC_NO AND BCD.DELETE_MARK = 'N'
                   WHERE BRA.DELETE_MARK='N' AND BCD.REC_ID IN ('I','K','KA','L','M','P','Q')),
                   
                   
                   SOT AS (
                   SELECT a.SOT_NO,a.SOT_SID,a.CUST_ID,a.PROD_ID,b.TYPE_ID,b.KIND_ID,b.COST_CENTER,b.CAMP_TARGET,a.SOT_AMT,a.SBK_AMT,a.QTY,a.SOT_DATE,a.CHANNEL_ID
                   FROM [B2CDB01].[dbo].[BC_SOT_DETAIL] a
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_STOCK_MASTER] b ON a.PROD_ID=b.PROD_ID AND b.DELETE_MARK='N'
                   LEFT OUTER JOIN SBK ON a.SOT_NO=SBK.SOT_NO AND a.SOT_SID=SBK.SOT_SID
                   WHERE a.DELETE_MARK='N' AND a.STATUS_ID NOT IN ('6','7') AND SBK.DOC_TYPE IS NULL
                   AND a.SOT_DATE < '2018-08-01'),
                   
                   Total_Order_Amt_BWG AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS Total_Order_Amt_BWG
                   FROM SOT
                   GROUP BY CUST_ID),
                   
                   Mag_Order_Amt_BWG AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS Mag_Order_Amt_BWG
                   FROM SOT
                   WHERE TYPE_ID='2'
                   GROUP BY CUST_ID),
                   
                   Act_Order_Amt_BWG AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS Act_Order_Amt_BWG
                   FROM SOT
                   WHERE TYPE_ID='3'
                   GROUP BY CUST_ID),
                   
                   Others_Order_Amt_BWG AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS Others_Order_Amt_BWG
                   FROM SOT
                   WHERE TYPE_ID='1'
                   GROUP BY CUST_ID),
                   
                   Total_Order_Amt_BW AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS Total_Order_Amt_BW
                   FROM SOT
                   WHERE COST_CENTER IN ('021','023')
                   GROUP BY CUST_ID),
                   
                   Total_Order_Amt_ST AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS Total_Order_Amt_ST
                   FROM SOT
                   WHERE COST_CENTER='022'
                   GROUP BY CUST_ID),
                   
                   Total_Order_Amt_Life AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS Total_Order_Amt_Life
                   FROM SOT
                   WHERE COST_CENTER='024'
                   GROUP BY CUST_ID),
                   
                   BW_Order_Amt_Adult AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS BW_Order_Amt_Adult
                   FROM SOT
                   WHERE COST_CENTER IN ('021','023') AND TYPE_ID='3' AND CAMP_TARGET='01'
                   GROUP BY CUST_ID),
                   
                   BW_Order_Count_Adult AS (
                   SELECT b.CUST_ID,COUNT(DISTINCT SOT.PROD_ID) AS BW_Order_Count_Adult
                   FROM SOT
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_CAMP_DETAIL] b ON SOT.SOT_NO=b.SOT_NO AND SOT.SOT_SID=b.SOT_SID AND b.DELETE_MARK='N'
                   WHERE COST_CENTER IN ('021','023') AND TYPE_ID='3' AND CAMP_TARGET='01' AND b.STATUS_ID NOT IN ('3','7')
                   GROUP BY b.CUST_ID),
                   
                   BW_Order_Amt_Stu AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS BW_Order_Amt_Stu
                   FROM SOT
                   WHERE COST_CENTER IN ('021','023') AND TYPE_ID='3' AND CAMP_TARGET IN ('021','022','023','031','032','033')
                   GROUP BY CUST_ID),
                   
                   BW_Order_Count_Stu AS (
                   SELECT b.CUST_ID,COUNT(DISTINCT SOT.PROD_ID) AS BW_Order_Count_Stu
                   FROM SOT
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_CAMP_DETAIL] b ON SOT.SOT_NO=b.SOT_NO AND SOT.SOT_SID=b.SOT_SID AND b.DELETE_MARK='N'
                   WHERE COST_CENTER IN ('021','023') AND TYPE_ID='3' AND CAMP_TARGET IN ('021','022','023','031','032','033') AND b.STATUS_ID NOT IN ('3','7')
                   GROUP BY b.CUST_ID),
                   
                   ST_Order_Amt_Adult AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS ST_Order_Amt_Adult
                   FROM SOT
                   WHERE COST_CENTER IN ('022') AND TYPE_ID='3'
                   GROUP BY CUST_ID),
                   
                   ST_Order_Count_Adult AS (
                   SELECT b.CUST_ID,COUNT(DISTINCT SOT.PROD_ID) AS ST_Order_Count_Adult
                   FROM SOT
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_CAMP_DETAIL] b ON SOT.SOT_NO=b.SOT_NO AND SOT.SOT_SID=b.SOT_SID AND b.DELETE_MARK='N'
                   WHERE COST_CENTER IN ('022') AND TYPE_ID='3' AND b.STATUS_ID NOT IN ('3','7')
                   GROUP BY b.CUST_ID),
                   
                   Alive_Order_Amt_Adult AS (
                   SELECT CUST_ID,SUM(SOT_AMT-SBK_AMT) AS Alive_Order_Amt_Adult
                   FROM SOT
                   WHERE COST_CENTER IN ('024') AND TYPE_ID='3'
                   GROUP BY CUST_ID),
                   
                   Alive_Order_Count_Adult AS (
                   SELECT b.CUST_ID,COUNT(DISTINCT SOT.PROD_ID) AS Alive_Order_Count_Adult
                   FROM SOT
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_CAMP_DETAIL] b ON SOT.SOT_NO=b.SOT_NO AND SOT.SOT_SID=b.SOT_SID AND b.DELETE_MARK='N'
                   WHERE COST_CENTER IN ('024') AND TYPE_ID='3' AND b.STATUS_ID NOT IN ('3','7')
                   GROUP BY b.CUST_ID),
                   
                   AD_PR AS (
                   SELECT a.[CUST_ID],MAX(a.[PR]) AS PR
                   FROM [CRC].[dbo].[Address_income] a
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_ODR_DELIVERY] b ON a.CUST_ID=b.CUST_ID AND a.ADDRESS_ID=b.ADDRESS_ID AND b.DELETE_MARK='N'
                   WHERE a.PR IS NOT NULL
                   GROUP BY a.CUST_ID),
                   
                   HAS_ONLINE_ORDER AS (
                   SELECT DISTINCT(CUST_ID), 1 HAS_ONLINE_ORDER
                   FROM SOT LEFT JOIN ONLINE_ORDER ON SOT.SOT_NO = ONLINE_ORDER.SOT_NO
                   WHERE ONLINE_ORDER.IF_ONLINE_ORDER = 1
                   ),
                   
                   NONBOOK AS(
                   SELECT *   
                   FROM [B2CDB01].[dbo].[BC_STOCK_MASTER]
                   WHERE DELETE_MARK='N' AND CLASSID1='03' AND CLASSID2='03'AND CLASSID3='01' --只含CLASS為紙本刊圖,無知識庫或dvd,但有電子雜誌
                   AND PROD_ID NOT LIKE 'SIMB____' --早期ST密技書
                   AND PROD_ID NOT LIKE 'SG____' --密技單期
                   AND PROD_ID NOT LIKE 'WMSI____A1' --ST單期
                   AND PROD_ID NOT LIKE 'WMSI____A1-001'--ST電單期
                   AND PROD_ID NOT LIKE 'SIMM___' --更早期ST單期
                   AND PROD_ID NOT LIKE 'ST____'--ST單期
                   AND PROD_ID NOT LIKE 'ES____'--ST電單期
                   AND PROD_ID NOT LIKE 'EBL____' --alive電單期
                   AND PROD_ID NOT LIKE 'EB____' --BW電單期
                   AND PROD_ID NOT LIKE 'BWL____' --alive單期
                   AND PROD_ID NOT LIKE 'BWLS____' --alive特別號單期
                   AND PROD_ID NOT LIKE 'BW____' --商周單期或單篇
                   AND PROD_ID NOT LIKE 'BWA%' --商周單篇
                   AND PROD_ID NOT LIKE 'BF%' --各種刊的單期
                   AND PROD_ID NOT LIKE 'GFMM%'
                   AND PROD_ID NOT LIKE 'GF0%'
                   AND NAME NOT LIKE '%商業周刊%'
                   AND NAME NOT LIKE '%商周%'
                   AND NAME NOT LIKE '%BW%' --BW沒有例外, ST有
                   AND NAME NOT LIKE '%GF%'
                   AND NAME NOT LIKE 'ST%期'
                   AND NAME NOT LIKE 'BW%期'
                   AND NAME_INVOICE NOT LIKE '%商業周刊%'
                   ),
                   
                   ITEM_COUNT_BIZBOOK AS(               
                   SELECT CUST_ID, COUNT(DISTINCT a.PROD_ID) AS ITEM_COUNT_BIZBOOK
                   FROM [B2CDB01].[dbo].[BC_SOT_DETAIL] a left join NONBOOK b on a.PROD_ID = b.PROD_ID
                   where a.DELETE_MARK = 'N' and a.STATUS_ID <> '6' and SOT_AMT > 0 and b.CLASSID1  is not null 
                   and CLASSID1 = '03' and CLASSID2 = '03' and CLASSID3 = '01' and CLASSID4 = '01'
                   GROUP BY CUST_ID),
                   
                   ITEM_COUNT_ARTBOOK AS (               
                   SELECT CUST_ID, COUNT(DISTINCT a.PROD_ID) AS ITEM_COUNT_ARTBOOK
                   FROM [B2CDB01].[dbo].[BC_SOT_DETAIL] a left join NONBOOK b on a.PROD_ID = b.PROD_ID
                   where a.DELETE_MARK = 'N' and a.STATUS_ID <> '6' and SOT_AMT > 0 and b.CLASSID1  is not null 
                   and CLASSID1 = '03' and CLASSID2 = '03' and CLASSID3 = '01' and CLASSID4 IN ('03','10')
                   GROUP BY CUST_ID),
                   
                   TICKET_ORDER_BEFORE AS(
                   SELECT CUST_ID,COUNT(DISTINCT SOT_NO) AS TICKET_ORDER_BEFORE
                   FROM SOT
                   WHERE PROD_ID in ('05AL180101','05AL180102','05AL201701','05AL201702','05AL201705','05AL201706','05AL201707','05AL201708','05AL201709','05AL201710','05BW201601','05BW201602','05BW201701','05BW201702','05BW201703','05BW201704','05BW201705','05BW201706','05BW201707','05BW201708','05BW201709','05BW201710','05BW201711','05BW201712','05BW201713','05BW201801','05BW201803','05BW20180701','05BW20180702','05BW20180703','05BW20180704','05BW20180705','05BW20180706','05BW20180707','05BW20180708','05BW20180709','05BW201812','05BW201814','05BW201815','05BW201816','05BW201817','05BW201818','05BW201823','05LB181901','05LB181902','05LB181903','05LB190101','05LB190102','05LB190201','05LB190301','05LB190401','05LB190501','05LB190601','05LB190701','05LB190801','05LB201706','05LB201707','05LB201708','05LB201709','05LB20170902','05LB20170903','05LB201710','05LB201712','05LB201801','05LB201802','05LB201803','05LB201804','05LB201805','05LB201806','05LB201807','05LB201808','05LB201809','05LB201810','05LB201811','05LB201812','05LB201818','05LB201820','05LB201821','05ST190201','05ST190301','05ST201701','05ST201702','05ST201801','05ST201802','05ST201803','05ST201804')
                   
                   GROUP BY CUST_ID),
                   
                   BOOK_ORDER_BEFORE AS(
                   SELECT CUST_ID,COUNT(DISTINCT SOT_NO) AS BOOK_ORDER_BEFORE
                   FROM SOT
                   WHERE PROD_ID in ('06BW190101','06BW201701','06BW201702','06BW20180','06BW201801','06BW201802','06BW201803','06BW201804','06BW201805','06BW201806','06BW201807','06BW201808')
                   GROUP BY CUST_ID),
                   
                   OTHER_ORDER_BEFORE AS(
                   SELECT CUST_ID,COUNT(DISTINCT SOT_NO) AS OTHER_ORDER_BEFORE
                   FROM SOT
                   WHERE PROD_ID in ('05AL201601','05AL201602','05AL201703','05AL201704','05BW181101','05BW181102','05BW190101','05BW190102','05BW190201','05BW201505','05BW201802','05BW201804','05BW201805','05BW201806','05BW201808','05BW201809','05BW201810','05BW201811','05BW201813','05BW201819','05BW201820','05BW201821','05BW201822','05BW201824','05BW201825','05BW201826','05BW201827','05LB201701','05LB201702','05LB201703','05LB201704','05LB201705','05LB201711','05LB201813','05LB201814','05LB201815','05LB201816','05LB201817','05LB201819','05ST190101','05ST190102','05ST190103')
                   GROUP BY CUST_ID),
                   
                   CROSS_BEFORE_DATA AS(
                   SELECT BC_ODR_MASTER.CUST_ID,TICKET_ORDER_BEFORE.TICKET_ORDER_BEFORE,BOOK_ORDER_BEFORE.BOOK_ORDER_BEFORE,OTHER_ORDER_BEFORE.OTHER_ORDER_BEFORE
                   FROM BC_ODR_MASTER 
                   LEFT JOIN TICKET_ORDER_BEFORE ON BC_ODR_MASTER.CUST_ID = TICKET_ORDER_BEFORE.CUST_ID
                   LEFT JOIN BOOK_ORDER_BEFORE ON BC_ODR_MASTER.CUST_ID = BOOK_ORDER_BEFORE.CUST_ID
                   LEFT JOIN OTHER_ORDER_BEFORE ON BC_ODR_MASTER.CUST_ID = OTHER_ORDER_BEFORE.CUST_ID
                   WHERE DELETE_MARK = 'N')
                   
                   
                   SELECT a.CUST_ID
                   ,a.CUST_NAME
                   ,a.CUST_COMPANY
                   ,d.CITY
                   ,d.AREA_NO
                   ,a.EDUCATION
                   ,a.GRADE
                   ,a.OCCUPATIONAL
                   ,convert(char(10), a.BIRTH_DATE, 120) AS BIRTH_DATE
                   ,DATEDIFF(YEAR,a.BIRTH_DATE,GETDATE()) AS Age
                   ,a.MARRIED
                   ,a.CHILDREN
                   ,a.CHILDREN_NUM
                   ,a.GUI_NO
                   ,a.CUST_TYPE
                   ,a.GENDER
                   ,(CASE WHEN e.ADDR_CITY_TOWN IS NOT NULL THEN e.ADDR_CITY_TOWN ELSE '' END)+
                   (CASE WHEN e.ADDR_VILLAGE IS NOT NULL THEN e.ADDR_VILLAGE ELSE '' END)+
                   (CASE WHEN e.ADDR_STREET_NO IS NOT NULL THEN e.ADDR_STREET_NO ELSE '' END) AS [ADDRESS]
                   ,(CASE WHEN f.ADDR_CITY_TOWN IS NOT NULL THEN f.ADDR_CITY_TOWN ELSE '' END)+
                   (CASE WHEN f.ADDR_VILLAGE IS NOT NULL THEN f.ADDR_VILLAGE ELSE '' END)+
                   (CASE WHEN f.ADDR_STREET_NO IS NOT NULL THEN f.ADDR_STREET_NO ELSE '' END) AS C_ADDRESS
                   ,(CASE WHEN fg.F_ADD3 IS NOT NULL THEN fg.F_ADD3 ELSE '' END)+
                   (CASE WHEN fg.F_ADD2 IS NOT NULL THEN fg.F_ADD2 ELSE '' END)+
                   (CASE WHEN fg.F_ADD1 IS NOT NULL THEN fg.F_ADD1 ELSE '' END) AS F_ADDRESS
                   ,a.MAIL_TO
                   ,a.TEL_P
                   ,a.TEL_O
                   ,a.CELLPHONE
                   ,a.EMAIL
                   ,a.HasSecretary
                   ,a.HasDriver
                   ,a.CarBrand
                   ,a.VIPContact
                   ,a.IsGrandRealEstate
                   ,a.IsGrandCreditCard
                   ,Total_Order_Amt_BWG.Total_Order_Amt_BWG
                   ,Mag_Order_Amt_BWG.Mag_Order_Amt_BWG
                   ,Act_Order_Amt_BWG.Act_Order_Amt_BWG
                   ,Others_Order_Amt_BWG.Others_Order_Amt_BWG
                   ,Total_Order_Amt_BW.Total_Order_Amt_BW
                   ,Total_Order_Amt_ST.Total_Order_Amt_ST
                   ,Total_Order_Amt_Life.Total_Order_Amt_Life
                   ,BW_Order_Amt_Adult.BW_Order_Amt_Adult
                   ,BW_Order_Count_Adult.BW_Order_Count_Adult
                   ,BW_Order_Amt_Stu.BW_Order_Amt_Stu
                   ,BW_Order_Count_Stu.BW_Order_Count_Stu
                   ,ST_Order_Amt_Adult.ST_Order_Amt_Adult
                   ,ST_Order_Count_Adult.ST_Order_Count_Adult
                   ,Alive_Order_Amt_Adult.Alive_Order_Amt_Adult
                   ,Alive_Order_Count_Adult.Alive_Order_Count_Adult                   
                   ,ITEM_COUNT_BIZBOOK.ITEM_COUNT_BIZBOOK
                   ,ITEM_COUNT_ARTBOOK.ITEM_COUNT_ARTBOOK
                   ,CROSS_BEFORE_DATA.TICKET_ORDER_BEFORE
                   ,CROSS_BEFORE_DATA.BOOK_ORDER_BEFORE
                   ,CROSS_BEFORE_DATA.OTHER_ORDER_BEFORE
                   ,(CASE WHEN g.ODR_TYPE ='1' and i.ODR_TYPE = '1' THEN '未訂'
                   WHEN g.ODR_TYPE IS NULL and i.ODR_TYPE IS NULL THEN '未訂'
                   WHEN g.ODR_TYPE IS NULL and i.ODR_TYPE = '1' THEN '未訂'
                   WHEN g.ODR_TYPE = '1' and i.ODR_TYPE IS NULL THEN '未訂'
                   WHEN g.ODR_TYPE ='3' or i.ODR_TYPE = '3' THEN '現訂' 
                   WHEN g.ODR_TYPE ='5' or i.ODR_TYPE = '5' THEN '停寄'
                   ELSE '斷訂' END) AS Order_State_Bw_PE_Mg
                   ,BW_tenure.BW_total_tenure AS Order_Tenure_Bw_PE_Mg
                   ,(CASE WHEN g.ODR_TYPE ='1' THEN '未訂'
                   WHEN g.ODR_TYPE IS NULL THEN '未訂'
                   WHEN g.ODR_TYPE ='3' THEN '現訂' 
                   WHEN g.ODR_TYPE ='5' THEN '停寄'
                   ELSE '斷訂' END) AS Order_State_Bw_Mg
                   ,(CASE WHEN h.ODR_TYPE ='1' and j.ODR_TYPE = '1' THEN '未訂'
                   WHEN h.ODR_TYPE IS NULL and j.ODR_TYPE IS NULL THEN '未訂'
                   WHEN h.ODR_TYPE IS NULL and j.ODR_TYPE = '1' THEN '未訂'
                   WHEN h.ODR_TYPE = '1' and j.ODR_TYPE IS NULL THEN '未訂'
                   WHEN h.ODR_TYPE ='3' or j.ODR_TYPE = '3' THEN '現訂' 
                   WHEN h.ODR_TYPE ='5' or j.ODR_TYPE = '5' THEN '停寄'
                   ELSE '斷訂' END) AS Order_State_ST_PE_Mg
                   ,ST_tenure.Smart_total_tenure AS Order_Tenure_ST_PE_Mg
                   ,(CASE WHEN h.ODR_TYPE ='1' THEN '未訂'
                   WHEN h.ODR_TYPE IS NULL THEN '未訂'
                   WHEN h.ODR_TYPE ='3' THEN '現訂' 
                   WHEN h.ODR_TYPE ='5' THEN '停寄'
                   ELSE '斷訂' END) AS Order_State_ST_Mg
                   ,(CASE WHEN i.ODR_TYPE ='1' THEN '未訂'
                   WHEN i.ODR_TYPE IS NULL THEN '未訂'
                   WHEN i.ODR_TYPE ='3' THEN '現訂' 
                   WHEN i.ODR_TYPE ='5' THEN '停寄'
                   ELSE '斷訂' END) AS Order_State_EMGBW
                   ,i.TOTAL_YEARS1 AS Order_Tenure_EMGBW
                   ,(CASE WHEN j.ODR_TYPE ='1' THEN '未訂'
                   WHEN j.ODR_TYPE IS NULL THEN '未訂'
                   WHEN j.ODR_TYPE ='3' THEN '現訂' 
                   WHEN j.ODR_TYPE ='5' THEN '停寄'
                   ELSE '斷訂' END) AS Order_State_EMGST
                   ,j.TOTAL_YEARS1 AS Order_Tenure_EMGST
                   ,(CASE WHEN k.ODR_TYPE ='1' THEN '未訂'
                   WHEN k.ODR_TYPE IS NULL THEN '未訂'
                   WHEN k.ODR_TYPE ='3' THEN '現訂' 
                   WHEN k.ODR_TYPE ='5' THEN '停寄'
                   ELSE '斷訂' END) AS Order_State_GOLF
                   ,AD_PR.PR
                   ,HAS_ONLINE_ORDER.HAS_ONLINE_ORDER
                   ,(case when WEB_ACCOUNT_ID = '' or WEB_ACCOUNT_ID is NULL THEN 0 ELSE 1 END) AS HAS_WEB_ACCOUNT
                   ,(case when EMAIL = '' or EMAIL is NULL THEN 0 ELSE 1 END) AS HAS_EMAIL
                   
                   FROM [B2CDB01].[dbo].[BC_ODR_MASTER] a
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_ODR_DELIVERY] c ON (CASE WHEN a.MAIL_TO='1' THEN a.ADDRESS_ID
                   WHEN a.MAIL_TO='2' THEN a.F_ADDRESS_ID
                   WHEN a.MAIL_TO='3' THEN a.C_ADDRESS_ID END)=c.ADDRESS_ID AND a.CUST_ID=c.CUST_ID AND c.DELETE_MARK='N'
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_TW_ZIPCODE] d ON c.ZIP_NO_1=d.ZIP_NO_1 AND d.DELETE_MARK='N'
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_ODR_DELIVERY] e ON a.ADDRESS_ID=e.ADDRESS_ID AND a.CUST_ID=e.CUST_ID AND e.DELETE_MARK='N'
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_ODR_DELIVERY] f ON a.C_ADDRESS_ID=f.ADDRESS_ID AND a.CUST_ID=f.CUST_ID AND f.DELETE_MARK='N'
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_ODR_DELIVERY] fg ON a.C_ADDRESS_ID=fg.ADDRESS_ID AND a.CUST_ID=fg.CUST_ID AND fg.DELETE_MARK='N'
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_ODR_PROFILE] g ON a.CUST_ID=g.CUST_ID AND g.PROFILE_ID='101'
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_ODR_PROFILE] h ON a.CUST_ID=h.CUST_ID AND h.PROFILE_ID='201'
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_ODR_PROFILE] i ON a.CUST_ID=i.CUST_ID AND i.PROFILE_ID='103'
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_ODR_PROFILE] j ON a.CUST_ID=j.CUST_ID AND j.PROFILE_ID='202'
                   LEFT OUTER JOIN [B2CDB01].[dbo].[BC_ODR_PROFILE] k ON a.CUST_ID=k.CUST_ID AND k.PROFILE_ID='105'
                   LEFT OUTER JOIN HAS_ONLINE_ORDER  ON a.CUST_ID = HAS_ONLINE_ORDER.CUST_ID                   
                   LEFT OUTER JOIN Total_Order_Amt_BWG ON a.CUST_ID=Total_Order_Amt_BWG.CUST_ID
                   LEFT OUTER JOIN Mag_Order_Amt_BWG ON a.CUST_ID=Mag_Order_Amt_BWG.CUST_ID
                   LEFT OUTER JOIN Act_Order_Amt_BWG ON a.CUST_ID=Act_Order_Amt_BWG.CUST_ID
                   LEFT OUTER JOIN Others_Order_Amt_BWG ON a.CUST_ID=Others_Order_Amt_BWG.CUST_ID
                   LEFT OUTER JOIN Total_Order_Amt_BW ON a.CUST_ID=Total_Order_Amt_BW.CUST_ID
                   LEFT OUTER JOIN Total_Order_Amt_ST ON a.CUST_ID=Total_Order_Amt_ST.CUST_ID
                   LEFT OUTER JOIN Total_Order_Amt_Life ON a.CUST_ID=Total_Order_Amt_Life.CUST_ID
                   LEFT OUTER JOIN BW_Order_Amt_Adult ON a.CUST_ID=BW_Order_Amt_Adult.CUST_ID
                   LEFT OUTER JOIN BW_Order_Count_Adult ON a.CUST_ID=BW_Order_Count_Adult.CUST_ID
                   LEFT OUTER JOIN BW_Order_Amt_Stu ON a.CUST_ID=BW_Order_Amt_Stu.CUST_ID
                   LEFT OUTER JOIN BW_Order_Count_Stu ON a.CUST_ID=BW_Order_Count_Stu.CUST_ID
                   LEFT OUTER JOIN ST_Order_Amt_Adult ON a.CUST_ID=ST_Order_Amt_Adult.CUST_ID
                   LEFT OUTER JOIN ST_Order_Count_Adult ON a.CUST_ID=ST_Order_Count_Adult.CUST_ID
                   LEFT OUTER JOIN Alive_Order_Amt_Adult ON a.CUST_ID=Alive_Order_Amt_Adult.CUST_ID
                   LEFT OUTER JOIN Alive_Order_Count_Adult ON a.CUST_ID=Alive_Order_Count_Adult.CUST_ID
                   LEFT OUTER JOIN [CRC].[dbo].[BW_tenure] BW_tenure ON a.CUST_ID=BW_tenure.CUST_ID
                   LEFT OUTER JOIN [CRC].[dbo].[ST_tenure] ST_tenure ON a.CUST_ID=ST_tenure.CUST_ID
                   LEFT OUTER JOIN [AD_PR] ON a.CUST_ID=AD_PR.CUST_ID
                   LEFT OUTER JOIN ITEM_COUNT_BIZBOOK on a.CUST_ID = ITEM_COUNT_BIZBOOK.CUST_ID
                   LEFT OUTER JOIN ITEM_COUNT_ARTBOOK on a.CUST_ID = ITEM_COUNT_ARTBOOK.CUST_ID
                   LEFT OUTER JOIN CROSS_BEFORE_DATA on a.CUST_ID = CROSS_BEFORE_DATA.CUST_ID
                   WHERE a.DELETE_MARK='N'",stringsAsFactors=FALSE, as.is = T)
  CRM$CUST_ID <- as.character(CRM$CUST_ID)
  
  CRM <- left_join(CRM,Last_tax_ID,by = c("CUST_ID"="CUST_ID"))
  CRM <- left_join(CRM,Newest_tax_ID,by = c("CUST_ID"="CUST_ID"))
  CRM <- left_join(CRM,Newest_tax_ID_true,by = c("CUST_ID"="CUST_ID"))
  CRM <- left_join(CRM,Latest_Order_Date_BWG,by = c("CUST_ID"="CUST_ID"))
  CRM$Latest_Order_Date_BWG <- ymd(CRM$Latest_Order_Date_BWG)
  CRM$Latest_Order_From_180801 <- as.Date("2018-08-01") - CRM$Latest_Order_Date_BWG 
  CRM$Latest_Order_From_180801 <- as.numeric(CRM$Latest_Order_From_180801)
  CRM <- left_join(CRM,Latest_Order_Channel_BWG,by = c("CUST_ID"="CUST_ID"))
  CRM$IF_USE_TAX <- ifelse(is.na(CRM$Newest_tax_ID_true), "0", "1") %>% factor()
  CRM <- left_join(CRM,Latest_Mag_Bundle,by = c("CUST_ID"="CUST_ID"))
  CRM <- left_join(CRM,Latest_Mag_Units,by = c("CUST_ID"="CUST_ID"))
  
  for(i in 1:length(CRM)){
    if(is.character(CRM[,i])==1){
      CRM[,i] <- str_trim(CRM[,i],side='both')
      CRM[,i] <- ifelse(as.character(CRM[,i]) %in% c("", "98", "        "), NA, as.character(CRM[,i]))
    }else{CRM[,i] <- CRM[,i]}
  }
  
  #numerical value: count,amount change from NA to 0
  for(i in seq(1:length(CRM))[str_detect(names(CRM), or("Count","COUNT", "Amt","ORDER"))]){
    CRM[,i] <- ifelse(is.na(CRM[,i]), 0, CRM[,i])
  }
  
  return(CRM)
}
CRM <- data(x)

# CRMBACKUP <- CRM

#欄位格式設定-----
CRM$AREA_NO <- factor(CRM$AREA_NO, levels = c("01", "02", "03", "04", "05"), labels = c("北部", "中部", "南部", "東部", "離島"))

CRM$MAIL_TO <- factor(CRM$MAIL_TO, levels = c("1", "2", "3"), labels = c("住家", "國外", "公司"))

CRM$EDUCATION <- factor(CRM$EDUCATION, levels = c("01", "02", "03", "04"),  labels = c("高中及以下", "專科", "大學", "研究所及以上"))

CRM$OCCUPATIONAL <- factor(CRM$OCCUPATIONAL, levels = c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22"),
                           labels = c('金融業','製造業','專業性服務業','一般性服務業','資訊科技','批發零售','貿易業','不動產業','媒體傳播','農林漁牧礦業','軍公教','其他行業','家管','學生','退休','保險業','營建業','能源業','住宿業','運輸倉儲業','餐飲業','醫療業'))


CRM$GRADE <- factor(CRM$GRADE, levels = c("01", "02", "03", "04", "05"), 
                    labels = c("企業負責人","高階主管","中階及基層主管","職員","自僱工作者"))

CRM$MARRIED <- factor(CRM$MARRIED, levels = c("01", "02"), labels = c("已婚", "單身"))
CRM$CHILDREN <- factor(CRM$CHILDREN,  levels = c("01", "02"), labels = c("是", "否"))

CRM$GENDER <- factor(CRM$GENDER,levels = c("01", "02"), labels =  c("男","女"))

CRM$CUST_TYPE <- factor(CRM$CUST_TYPE,  levels = c("01", "02"), labels = c("個人", "法人"))

CRM$BIRTH_DATE <- ymd(CRM$BIRTH_DATE)


CRM$Order_State_Bw_PE_Mg <- factor(CRM$Order_State_Bw_PE_Mg)
CRM$Order_State_Bw_Mg <- factor(CRM$Order_State_Bw_Mg)
CRM$Order_State_ST_PE_Mg <- factor(CRM$Order_State_ST_PE_Mg)
CRM$Order_State_ST_Mg <- factor(CRM$Order_State_ST_Mg)
CRM$Order_State_EMGBW <- factor(CRM$Order_State_EMGBW)
CRM$Order_State_EMGST <- factor(CRM$Order_State_EMGST)
CRM$Order_State_GOLF <- factor(CRM$Order_State_GOLF)
CRM$Latest_Order_Channel_BWG <- factor(CRM$Latest_Order_Channel_BWG)
CRM$IsGrandRealEstate <- factor(CRM$IsGrandRealEstate)
CRM$IsGrandCreditCard <- factor(CRM$IsGrandCreditCard)
CRM$CITY <- factor(CRM$CITY)
CRM$CHANNEL_NAME <- factor(CRM$CHANNEL_NAME)
CRM$CHANNEL_CATEGORY <- factor(CRM$CHANNEL_CATEGORY)
CRM$Latest_Mag_Bundle <- factor(CRM$Latest_Mag_Bundle)
CRM$HAS_ONLINE_ORDER <- factor(CRM$HAS_ONLINE_ORDER)
CRM$HAS_EMAIL <- factor(CRM$HAS_EMAIL)
CRM$HAS_WEB_ACCOUNT <- factor(CRM$HAS_WEB_ACCOUNT)

class(CRM$Latest_Mag_Units) <- "numeric"
class(CRM$Order_Tenure_Bw_PE_Mg) <- "numeric"
class(CRM$Order_Tenure_ST_PE_Mg) <- "numeric"
class(CRM$Order_Tenure_EMGBW) <- "numeric"
class(CRM$Order_Tenure_EMGST) <- "numeric"

class(CRM$PR) <- "numeric"

#-----------manipulate
conn <- odbcConnect("B2CDB", uid="80380", pwd="q6eXAYqB")
conn_83 <- odbcConnect("BWGDB", uid="80380", pwd="0932554572")

#----------------------------------------------1. CRM原始資料處理階段-------------------------------------------------------------
load("C:/Users/80380/Google 雲端硬碟/工作資料夾/企業資料庫開發程式碼/company_profile_190114.RData")
seller_uid <- openxlsx::read.xlsx("V:/1.0_CRM/07.需定期更新資料/直銷商名單.xlsx")

#執行定期更新資料中的CRMcode
CRM <- distinct(CRM)
n_distinct(CRM$CUST_ID) == nrow(CRM)

#1.電話分機改成可以跟黃頁簿mapping的
# tt <- CRM
# tt <- bind_cols(tt, as.data.frame(str_split(tt$Com_Phone, pattern = or("P","p"), simplify = T, n = 2))) 
# tt$Com_Phone_ext. <- as.character(tt$V2)
# tt$Com_Phone <- tt$V1
# tt <- select(tt, -V1, -V2)
# CRM <- tt

#2.做part_name,欄位:訂戶名、公司名、(住址->村里鄰之拿掉)
CRM$part_CUST_NAME<- "a"
CRM$part_companyname <- "a"

CRM$part_CUST_NAME<- str_replace_all(CRM$CUST_NAME, 
                                     or("事業","控股","股份","有限","公司", "分公司","(股)", "(股有)","[a-zA-Z]"), "") %>% 
  str_replace_all(pattern = or(PUNCT,SPC) %R% one_or_more(char_class(WRD, DGT, PUNCT, SPC)) %R% or(PUNCT,SPC) ,"" ) %>%   
  str_replace_all(pattern = or("[:punct:]", SPC, "[:digit:]", "\\+"), "") %>%
  str_replace_all("臺", "台")

CRM$part_companyname <- str_replace_all(CRM$CUST_COMPANY, 
                                        or("事業","控股","股份","有限","公司", "分公司","(股)", "(股有)","[a-zA-Z]"), "") %>% 
  str_replace_all(pattern = or(PUNCT,SPC) %R% one_or_more(char_class(WRD, DGT, PUNCT, SPC)) %R% or(PUNCT,SPC) ,"" ) %>%   
  str_replace_all(pattern = or("[:punct:]", SPC, "[:digit:]", "\\+"), "") %>%
  str_replace_all("臺", "台")



#-------------------------------------------2. 比對特殊機構, 做成special_unit----------------------------------
#特殊機構定義:非營利事業、可能沒統編、即使有統編也不會再財政部資料找到
#因為要橫跨兩個欄位做str_detect，所以先把NA都換成字串，之後再換回來
#company填特殊機構也算，假設他如果公司要填這些非營利機構，有很大機率是他是訂給公司看的
CRM$CUST_NAME<- str_replace_na(CRM$CUST_NAME)
CRM$CUST_COMPANY <- str_replace_na(CRM$CUST_COMPANY)

#醫院判斷
CRM$special_unit <- "N"
CRM$special_unit <- ifelse((str_detect(CRM$CUST_NAME, or("醫院", "醫學中心", "榮總")) == T | 
                              str_detect(CRM$CUST_COMPANY, or("醫院", "醫學中心", "榮總")) == T) , "醫院", CRM$special_unit)

#診所判斷
CRM$special_unit <- ifelse((str_detect(CRM$CUST_NAME, or("診所", "牙醫$", "中醫$")) == T | 
                              str_detect(CRM$CUST_COMPANY, or("診所", "牙醫$", "中醫$")) == T) , "診所", CRM$special_unit)
#圖書館判斷
CRM$special_unit <- ifelse((str_detect(CRM$CUST_NAME, or("立圖書館","公所圖書館","區圖書館","鄉圖書館")) == T | 
                              str_detect(CRM$CUST_COMPANY, or("立圖書館","公所圖書館","區圖書館","鄉圖書館")) == T) , "圖書館", CRM$special_unit)

#學校判斷
CRM$special_unit <- ifelse((str_detect(CRM$CUST_NAME, or("學校","國小","小學","國中","女中","中學","高中","一中","二中","高工","高職","高商","農工","家商","商工","工商$","工專","大學","科大","學院")) == T | 
                              str_detect(CRM$CUST_COMPANY, or("學校","國小","小學","國中","女中","中學","高中","一中","二中","高工","高職","高商","農工","家商","商工","工商$","工專","大學","科大","學院")) == T) , "學校", CRM$special_unit)

#政府單位判斷
CRM$special_unit <- ifelse((str_detect(CRM$CUST_NAME, or("政府","議會","法院","政院","公所","監察院","考試院","中華民國","局","委員會", "金融監督")) == T & 
                              !str_detect(CRM$CUST_NAME, or("福利委員會","職福委員會","職工委員會","稽核委員會","管理委員會","書局", "藥局", "郵局","公所圖書館"))== T)| 
                             (str_detect(CRM$CUST_COMPANY, or("政府","議會","法院","政院","公所","監察院","考試院","中華民國","局","委員會", "金融監督")) == T
                              & !str_detect(CRM$CUST_COMPANY, or("福利委員會","職福委員會","職工委員會","稽核委員會","管理委員會","書局", "藥局", "郵局","公所圖書館"))== T) , "政府單位", CRM$special_unit)

#財團法人/基金會判斷
CRM$special_unit <- ifelse((str_detect(CRM$CUST_NAME, or("^財團法人","基金會*")) == T & 
                              !str_detect(CRM$CUST_COMPANY, or("^財團法人","基金會*"))== T), "基金會",CRM$special_unit)  

#特殊公會判斷
CRM$special_unit <- ifelse((str_detect(CRM$CUST_NAME, or1("公會")) == T & 
                              !str_detect(CRM$CUST_COMPANY, or1("公會"))== T), "公會",CRM$special_unit)  


#----------------------------------------------3. 三大欄位比對-------------------------------------------------------------

#以本名來mapping公司統編，把名字不大於3字的刪掉，因為錯誤率太高(不是part_name，雖然也會出錯，但有些公司就是只有兩個關鍵字)，並挑出有成功的
#要把重複mapping到的名字捨棄掉
tt <- left_join(CRM, company_profile ,c("part_CUST_NAME" = "part_name"))
relay_CUST_NAME<- filter(tt, !CUST_NAME%in% row.names(table(tt$CUST_NAME)[table(tt$CUST_NAME) >=2])) %>% 
  filter(!is.na(company_uid)) %>% 
  filter(nchar(CUST_NAME) > 3) %>% 
  select(CUST_ID, company_uid)

names(relay_CUST_NAME) <- c("CUST_ID","membername_uid")

relay_CUST_NAME$membername_uid <- as.character(relay_CUST_NAME$membername_uid)


#以公司名稱來mapping公司統編
tt2 <- left_join(CRM, company_profile, c("part_companyname" = "part_name"))

relay_companyname <- filter(tt2, !CUST_COMPANY %in% row.names(table(tt2$CUST_COMPANY)[table(tt2$CUST_COMPANY) >=2])) %>% 
  filter(!is.na(company_uid)) %>% 
  select(CUST_ID, company_uid)
names(relay_companyname) <- c("CUST_ID","companyname_uid")
relay_companyname$companyname_uid <- as.character(relay_companyname$companyname_uid)


#以訂單公司電話來mapping公司統編，電話也會有一個電話多個公司用的問題，但找出這些公司代表找到公司關聯圖
CRM$TEL_O_clean <- str_replace_all(CRM$TEL_O, "p" %R% one_or_more(DGT), "")
relay_companyphone <- left_join(CRM, company_profile[!is.na(company_profile$company_phone),], c("TEL_O_clean" = "company_phone")) %>%
  filter(!is.na(company_uid)) %>%
  select(CUST_ID, company_uid)

names(relay_companyphone) <- c("CUST_ID","companyphone_uid")
relay_companyphone$companyphone_uid <- as.character(relay_companyphone$companyphone_uid)

#排除訂戶使用的公司電話有多家公司使用的狀況
relay_companyphone_clean <-  filter(relay_companyphone, !CUST_ID %in% row.names(table(relay_companyphone$CUST_ID)[table(relay_companyphone$CUST_ID) >=2]))



#-------------------------------------------4. 補充三大欄位uid，做成company_uid_true----------------------------------

#補上可用簡單方法補充的資料列
CRM_taxid3supple <- left_join(CRM["CUST_ID"], relay_CUST_NAME, "CUST_ID") %>% 
  left_join(relay_companyname, "CUST_ID") %>% 
  left_join(relay_companyphone_clean, "CUST_ID")

#此為可能可補充資料的欄位 順序:電話->公司名稱->個人名稱(越後面越準)
CRM_taxid3supple$comb_3uid <- "a"
CRM_taxid3supple$comb_3uid <- ifelse(!is.na(CRM_taxid3supple$companyphone_uid), CRM_taxid3supple$companyphone_uid, 
                                     ifelse(!is.na(CRM_taxid3supple$companyname_uid), CRM_taxid3supple$companyname_uid, CRM_taxid3supple$membername_uid)) %>% as.character() 

CRM_taxid3supple$comb_3uid[CRM_taxid3supple$comb_3uid %in% seller_uid$company_uid] <- NA #delete seller

#補充的uid整併到原始檔 以客戶自填維基礎 但1.if沒對到uid就換比對來的 2.if前數步驟後發現是直銷商就換
#最後一步，用預設統編補上(已排除預設是直接商或電商平台的)
#直銷商的，如果三大欄位無法幫比對出來，就會是NA=此步驟排除直銷商

CRM <- left_join(CRM, CRM_taxid3supple[c("CUST_ID", "comb_3uid")], "CUST_ID")
CRM$company_uid_true <- NA
CRM$company_uid_true <- ifelse(is.na(CRM$Newest_tax_ID_true) , CRM$comb_3uid, CRM$Newest_tax_ID_true)
CRM$company_uid_true <- ifelse(CRM$company_uid_true %in% seller_uid$company_uid , CRM$comb_3uid, CRM$company_uid_true)
CRM$company_uid_true[is.na(CRM$company_uid_true)
                     & !CRM$GUI_NO %in% seller_uid$company_uid] <- CRM$GUI_NO[is.na(CRM$company_uid_true)
                                                                              & !CRM$GUI_NO %in% seller_uid$company_uid]

CRM <- select(CRM, -TEL_O_clean, -part_companyname, -part_CUST_NAME, -comb_3uid)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~4.判別純法人/個人做成identity
#創造一個虛擬物件，只有ID跟名字
#把名字的空格刪掉
CUST_NAME_classify <- select(CRM, CUST_ID, CUST_NAME)
CUST_NAME_classify$CUST_NAME<- str_replace_all(CUST_NAME_classify$CUST_NAME, or(" ", "　"),"")
tt1 <- filter(CUST_NAME_classify, str_detect(CUST_NAME_classify$CUST_NAME, "[A-Za-z]+"), !str_detect(CUST_NAME_classify$CUST_NAME, "[\u4e00-\u9fa5]+"))
tt1_2 <- filter(CUST_NAME_classify, str_detect(CUST_NAME_classify$CUST_NAME, "[A-Za-z]+"), str_detect(CUST_NAME_classify$CUST_NAME, "[\u4e00-\u9fa5]+"))

CUST_NAME_classify$has_eng <- ifelse(CUST_NAME_classify$CUST_ID %in% tt1$CUST_ID, "e", ifelse(CUST_NAME_classify$CUST_ID %in% tt1_2$CUST_ID, "ce", "N"))

#大於3個字47415，小於等於3個字= 254502 - 47415= 207087
CUST_NAME_classify$over3word <- ifelse(nchar(CUST_NAME_classify$CUST_NAME) > 3, "Y", "N")
CUST_NAME_classify$unit <- ifelse(str_detect(CUST_NAME_classify$CUST_NAME, or("館","站","總務","店","部","室","課","屋","處","組","廠", "局") %R% END), "Y", "N")
CUST_NAME_classify$title <- ifelse(str_detect(CUST_NAME_classify$CUST_NAME, or("經理", "議員","委員", "幹事", "教授", "老師", "董事", "長", 
                                                                               "主任","顧問","副理", "協理", "主管","請勿拆閱\\)","代收\\)",
                                                                               "法師","女士","先生","小姐", "醫師") %R% END), "Y", "N")
#四字個人判別
name150 <- c("陳","林","黃","張","李","王","吳","劉","蔡","楊","許","鄭","謝","洪","郭","邱","曾","廖","賴","徐","周","葉","蘇","莊","呂","江","何","蕭","羅","高","潘","簡","朱","鍾","彭","游","詹","胡","施","沈","余","盧","梁","趙","顏","柯","翁","魏","孫","戴","范","方","宋","鄧","杜","傅","侯","曹","薛","丁","卓","馬","阮","董","唐","温","藍","蔣","石","古","紀","姚","連","馮","歐","程","湯","黄","田","康","姜","汪","白","鄒","尤","巫","鐘","黎","涂","龔","嚴","韓","袁","金","童","陸","夏","柳","凃","邵","錢","伍","倪","溫","于","譚","駱","熊","任","甘","秦","顧","毛","章","史","萬","官","俞","雷","饒","粘","張簡","闕","崔","凌","尹","孔","辛","歐陽","辜","陶","易","段","武","龍","韋","葛","池","孟","褚","殷","麥","賀","賈","莫","文","管","關","向","包")
namedouble <- c("張簡","歐陽","范姜","周黃","江謝","司徒","張廖","翁林","姜林","上官","朱陳","端木","邱黃","諸葛","葉劉","張李","皇甫","王李","阮呂","劉黃","林吳","陳吳","歐李","陳李","張許","申屠","尉遲","張陳","徐辜","陳黃","蘇陳","李陳","司馬","李林","郭李","徐陳","劉張","蔡黃","王劉","黃吳","洪許","李王","軒轅","張楊","夏侯","鮮于","曾林","謝林","陳許","余楊","陳辜","令狐","索南","黃沈","郭黃","李黃","顓孫","淳于","陳彭","鄭黃","吳游","吳鄭","莊吳","廖蔡","土登","方鄒","格桑","康張","陳呂","劉范","戴楊","濮陽","札西","朱翁","沈游","東方","郭洪","江周","第五","噶瑪","鍾張","旦增","李梁","陸費","劉許","孫林","徐謝","王顏","延陵","胡周","益西","貢佈","張趙","袞桑","連林","褚阮","扎西","朱吳","白瑪","張朱","曾江","劉郭","薛林","公羊","鍾巴","文樂","鐘簡","比嘉","王易","旦巴","伊西","江田","孫劉","班澤")

tt2 <- filter(CUST_NAME_classify, str_count(CUST_NAME) == 4 , 
              str_detect(CUST_NAME, START %R% or1(namedouble))
              | str_detect(CUST_NAME, START %R% or1(name150) %R% or1(name150)))
CUST_NAME_classify$ind_4words <- ifelse(CUST_NAME_classify$CUST_ID %in% tt2$CUST_ID, "Y", "N")


CUST_NAME_classify$identity <- "a"
CUST_NAME_classify$identity <- ifelse(CUST_NAME_classify$has_eng == "ce","純法人",
                                      ifelse(CUST_NAME_classify$has_eng == "N"& CUST_NAME_classify$over3word == "Y"& CUST_NAME_classify$title == "N","純法人", 
                                             ifelse(CUST_NAME_classify$has_eng == "N"& CUST_NAME_classify$over3word == "N"& CUST_NAME_classify$unit == "Y","純法人", "個人")))

CUST_NAME_classify$identity <- ifelse(CUST_NAME_classify$has_eng == "N" & str_count(CUST_NAME_classify$CUST_NAME) == 4 & CUST_NAME_classify$ind_4words == "Y", "個人", CUST_NAME_classify$identity)

CRM <- left_join(CRM, CUST_NAME_classify[c("CUST_ID", "identity")], "CUST_ID")

#企業負責人更新到職階上
#mapping訂戶中的企業負責人
representative <- left_join(CRM, company_profile, c("CUST_NAME" = "representative", "company_uid_true" = "company_uid"))
representative <- filter(representative, !is.na(company_name)) %>% 
  arrange(CUST_NAME, company_uid_true)
representative <- select(representative, CUST_ID, CUST_NAME, CUST_COMPANY,company_name, company_uid_true, employees,  reg_type)
names(representative) <- c("CUST_ID", "CUST_NAME", "自填公司名稱","正式登記公司名稱", "正式公司統編","公司員工數", "公司類型")

#已知是企業負責人的填補起來
CRM$Positions <- CRM$GRADE
CRM$Positions[CRM$CUST_ID %in% representative$CUST_ID] <- "企業負責人"

#mapping行業，用uid_true填補,應是以公司資料為主,自填補助
CRM <- left_join(CRM, company_profile[c("company_uid", "industry_category")], c("company_uid_true" = "company_uid"))
CRM$industry_category <- ifelse(is.na(CRM$industry_category), as.character(CRM$OCCUPATIONAL), CRM$industry_category)



#mapping公司營業額員工數, 使用uid_true盡可能mapping
CRM <- left_join(CRM, company_profile[c("company_uid", "employees", "sales")], c("company_uid_true" = "company_uid"))

#

CRM$identity <- factor(CRM$identity)
CRM$industry_category <- factor(CRM$industry_category)


#中間mapping的過程還是會mapping到重複的那個人
CRM <- distinct(CRM)

CRMDB <- CRM

# 
# 
save(CRMDB, file="CRMDB_CROSS_SELLING_190327.Rdata")

