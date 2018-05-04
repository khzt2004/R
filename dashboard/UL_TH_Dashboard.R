library(tidyverse)
library (bigrquery)
library (lubridate)

# Big Query Setting
project <- "unified-welder-172709"
targets_dataset <- "TH_Marketing_Performance_Dashboard_Targets"

# get targets data by date
get_target_query <- paste0("SELECT
                           Upload_Date, Country, Year, Month,Channel,Category,Partnership,Metric,Target
                           FROM (SELECT
                           Upload_Date,Country,Year,Month,Channel,Category,Partnership, 'NMV' AS Metric,
                           cast(SUM(NMV) as FLOAT64) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6,7,8)
                           UNION ALL
                           (SELECT
                           Upload_Date, Country,Year,Month,Channel,Category,Partnership, 'Item' AS Metric,
                           ROUND(cast(sum(Item) as FLOAT64),2) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6,7,8)
                           UNION ALL
                           (SELECT
                           Upload_Date, Country,Year,Month,Channel,Category,Partnership,
                           'ASP' AS Metric,
                           cast(SUM(ASP) as FLOAT64) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6,7,8)
                           UNION ALL
                           (SELECT
                           Upload_Date, Country,Year,Month,Channel,Category,Partnership,
                           'PV' AS Metric,ROUND(cast(SUM(PV) as FLOAT64),2) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6,7,8)
                           UNION ALL
                           (SELECT
                           Upload_Date, Country,Year,Month,Channel,Category,Partnership,
                           'CR' AS Metric,cast(CR as FLOAT64) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6,7,8,9 )
                           UNION ALL
                           (SELECT
                           Upload_Date, Country,Year,Month,Channel,Category,Partnership,
                           'NC' AS Metric, cast(SUM(NC) as FLOAT64) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6,7,8)
                           ")

target_data <- query_exec(get_target_query, project, destination_table = NULL, max_pages = Inf, use_legacy_sql = FALSE)

# get actuals data by date 
get_actuals_query <- paste0("WITH SKU_Detail_table AS (SELECT max(upload_date) as Ud, File_Date,	Partner,	Country,	Cat_Level_1,
Cat_Level_2,	Cat_Level_3,	Cat_Level_4,	Cat_Level_5,	Cat_Level_6,	Product_Name,	Brand,	SKU,	Brand_SKU,	
                            Retail_MP,	Seller_Name,	Seller_BOB_ID,	Lazada_Partner,	List_Price,	Current_Lazada_Price,	Competitor_Price,	
                            Competitive_Flag,	Competitor_URL,	Top_1_L7D,	Top_2_L7D,	Top_3_L7D,	Top_4_L7D,	Top_5_L7D,	Stock_Level,	Inventory_Coverage_in_Days,
                            Date_Period,	Customers,	Lazada_Own_Media,	Organic_Traffic,	Paid_Media,	Referrals,	Male,	Female,	Age_0_18,	Age_19_25,
                            Age_26_35,	Age_35_older,	Price,	Item_Order,	desktop,	iosApp,	androidApp,	mobile,	tablet,	COD,
                            E_Payment,	Bank_Transfer,	Credit_Card,	Urban,	Rural,	NMV,	Net_Items,	Net_Orders,	PV_Web,	CR_Web,	PV_App,	CR_App,	New_Cust
                            FROM `unified-welder-172709.Brand_Partnership_Report_Unilever_SEA.SKU_Detail_*`
                            WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                            AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) AND Date_Period = 'L7D' group by 2,3,4,5,6,7,8,9,10,
                            11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
                            48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63),
                            Shopee_Pdt_Perf_Table as (SELECT max(upload_date) as Ud, File_Date,	Partner,	Country,	Start_Date,	End_Date,	Retailer,
                            Item_Name,	Category,	Sub_Category,	Total_Quantity_Sold,	Stock_Available,	Days_before_OOS,	Restock_Action_Needed,
                            Suggested_Replenishment_1_week_coverage,	No_of_Unique_Buyers_Previous_Week,	No_of_Unique_Buyers_This_Week,	Completed_Orders_Previous_Week,
                            Completed_Orders_This_Week,	Completed_Sales_Previous_Week_Local_Currency,	Completed_Sales_This_Week_Local_Currency,
                            Original_Listing_Price_Local_Currency,	Average_Listing_Price_Local_Currency,	Total_Product_Views,	Cat_Level_1,	Cat_Level_2,
                            Cat_Level_3,	No_of_Unique_Buyers_WoW,	Completed_Orders_WoW,	Completed_Sales_WoW,	Average_Discount_Level,
                            Conversion_Rate,	Product_Return_Rate,	Local_Currency,	EUR_Forex_Rate,	USD_Forex_Rate,	Completed_Sales_Previous_Week_EUR,
                            Completed_Sales_Previous_Week_USD,	Completed_Sales_This_Week_EUR,	Completed_Sales_This_Week_USD,	Original_Listing_Price_EUR,
                            Original_Listing_Price_USD,	Average_Listing_Price_EUR,	Average_Listing_Price_USD,	Brand
                            FROM `unified-welder-172709.Regional_Official_Seller_Report.Product_Performance_data_*`
                            WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                            AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) group by 2,3,4,5,6,7,8,9,10,11,
                            12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45)
                            
                            
                            select 
                            Date_of_Week,
                            EXTRACT(Year from Date_of_Week) as Year,
                            case when extract(month from Date_of_Week) = 1 then 'Jan'
                            when extract(month from Date_of_Week) = 2 then 'Feb' 
                            when extract(month from Date_of_Week) = 3 then 'Mar'
                            when extract(month from Date_of_Week) = 4 then 'Apr'
                            when extract(month from Date_of_Week) = 5 then 'May'
                            when extract(month from Date_of_Week) = 6 then 'Jun'
                            when extract(month from Date_of_Week) = 7 then 'Jul'
                            when extract(month from Date_of_Week) = 8 then 'Aug'
                            when extract(month from Date_of_Week) = 9 then 'Sep'
                            when extract(month from Date_of_Week) = 10 then 'Oct'
                            when extract(month from Date_of_Week) = 11 then 'Nov'
                            when extract(month from Date_of_Week) = 12 then 'Dec'
                            end as Month,
                            extract(week from Date_of_Week) as Week,
                            Channel,
                            #  Brand,
                            Country,
                            Category,
                            # Sub_Category,
                            Partnership,
                            SUM(NMV) as NMV,
                            sum(Items) as Item,
                            safe_divide(sum(NMV),sum(Items)) as ASP,
                            sum(PV) as PV,
                            safe_divide(sum(Items), sum(PV)) as CR,
                            sum(NC) as NC
                            from(SELECT 'Lazada' as Channel,
                            Brand,Country,
                            case when Cat_Level_1 is null then 'Others' else Cat_Level_1 end as Category,
                            # case when Cat_Level_2 is null then 'Others' else Cat_Level_2 end as Sub_Category,
                            Retail_MP as Partnership,
                            cast(File_Date as DATE) AS Date_of_Week,
                            sum(Net_Items) as Items,
                            SUM(NMV) AS NMV,
                            sum(PV_App) + sum(PV_Web) as PV,
                            sum(New_Cust) as NC
                            FROM SKU_Detail_table
                            GROUP BY Country, Category,
                            # Sub_Category,
                            Partnership, Date_of_Week,  Channel, Brand
                            UNION ALL (  
                            SELECT 'Shopee' as Channel,
                            Brand,
                            Country,
                            case when Cat_Level_1 is null then 'Others' else Cat_Level_1 end as Category,
                            # case when Cat_Level_2 is null then 'Others' else Cat_Level_2 end as Sub_Category,
                            case when Country = 'TH' then 'MP' else 'Retail' end as Partnership,
                            # need retail/MP partnership
                            cast(File_Date as DATE) AS Date_of_Week,
                            sum(cast(Total_Quantity_Sold as FLOAT64)) as Items,
                            SUM(Completed_Sales_This_Week_EUR) AS NMV,
                            sum(Total_Product_Views) as PV,
                            cast('0' as INT64) as NC
                            FROM Shopee_Pdt_Perf_Table
                            GROUP BY Country, Category,
                            #  Sub_Category,
                            Date_of_Week,
                            Channel,
                            Brand,
                            Partnership))
                            group by Date_of_Week, Year, Month, Week, Channel,
                            # Brand, 
                            Country, Category,
                            # Sub_Category,
                            Partnership")

actuals_data <- query_exec(get_actuals_query, project, destination_table = NULL, max_pages = Inf, use_legacy_sql = FALSE)

# get actuals data by date 
get_actuals_subcat_query <- paste0("WITH SKU_Detail_table AS (SELECT max(upload_date) as Ud, File_Date,	Partner,	Country,	Cat_Level_1,
Cat_Level_2,	Cat_Level_3,	Cat_Level_4,	Cat_Level_5,	Cat_Level_6,	Product_Name,	Brand,	SKU,	Brand_SKU,	
                                   Retail_MP,	Seller_Name,	Seller_BOB_ID,	Lazada_Partner,	List_Price,	Current_Lazada_Price,	Competitor_Price,	
                                   Competitive_Flag,	Competitor_URL,	Top_1_L7D,	Top_2_L7D,	Top_3_L7D,	Top_4_L7D,	Top_5_L7D,	Stock_Level,	Inventory_Coverage_in_Days,
                                   Date_Period,	Customers,	Lazada_Own_Media,	Organic_Traffic,	Paid_Media,	Referrals,	Male,	Female,	Age_0_18,	Age_19_25,
                                   Age_26_35,	Age_35_older,	Price,	Item_Order,	desktop,	iosApp,	androidApp,	mobile,	tablet,	COD,
                                   E_Payment,	Bank_Transfer,	Credit_Card,	Urban,	Rural,	NMV,	Net_Items,	Net_Orders,	PV_Web,	CR_Web,	PV_App,	CR_App,	New_Cust
                                   FROM `unified-welder-172709.Brand_Partnership_Report_Unilever_SEA.SKU_Detail_*`
                                   WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                                   AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) AND Date_Period = 'L7D' group by 2,3,4,5,6,7,8,9,10,
                                   11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
                                   48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63),
                                   Shopee_Pdt_Perf_Table as (SELECT max(upload_date) as Ud, File_Date,	Partner,	Country,	Start_Date,	End_Date,	Retailer,
                                   Item_Name,	Category,	Sub_Category,	Total_Quantity_Sold,	Stock_Available,	Days_before_OOS,	Restock_Action_Needed,
                                   Suggested_Replenishment_1_week_coverage,	No_of_Unique_Buyers_Previous_Week,	No_of_Unique_Buyers_This_Week,	Completed_Orders_Previous_Week,
                                   Completed_Orders_This_Week,	Completed_Sales_Previous_Week_Local_Currency,	Completed_Sales_This_Week_Local_Currency,
                                   Original_Listing_Price_Local_Currency,	Average_Listing_Price_Local_Currency,	Total_Product_Views,	Cat_Level_1,	Cat_Level_2,
                                   Cat_Level_3,	No_of_Unique_Buyers_WoW,	Completed_Orders_WoW,	Completed_Sales_WoW,	Average_Discount_Level,
                                   Conversion_Rate,	Product_Return_Rate,	Local_Currency,	EUR_Forex_Rate,	USD_Forex_Rate,	Completed_Sales_Previous_Week_EUR,
                                   Completed_Sales_Previous_Week_USD,	Completed_Sales_This_Week_EUR,	Completed_Sales_This_Week_USD,	Original_Listing_Price_EUR,
                                   Original_Listing_Price_USD,	Average_Listing_Price_EUR,	Average_Listing_Price_USD,	Brand
                                   FROM `unified-welder-172709.Regional_Official_Seller_Report.Product_Performance_data_*`
                                   WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                                   AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) group by 2,3,4,5,6,7,8,9,10,11,
                                   12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45)
                                   
                                   
                                   select 
                                   Date_of_Week,
                                   EXTRACT(Year from Date_of_Week) as Year,
                                   case when extract(month from Date_of_Week) = 1 then 'Jan'
                                   when extract(month from Date_of_Week) = 2 then 'Feb' 
                                   when extract(month from Date_of_Week) = 3 then 'Mar'
                                   when extract(month from Date_of_Week) = 4 then 'Apr'
                                   when extract(month from Date_of_Week) = 5 then 'May'
                                   when extract(month from Date_of_Week) = 6 then 'Jun'
                                   when extract(month from Date_of_Week) = 7 then 'Jul'
                                   when extract(month from Date_of_Week) = 8 then 'Aug'
                                   when extract(month from Date_of_Week) = 9 then 'Sep'
                                   when extract(month from Date_of_Week) = 10 then 'Oct'
                                   when extract(month from Date_of_Week) = 11 then 'Nov'
                                   when extract(month from Date_of_Week) = 12 then 'Dec'
                                   end as Month,
                                   extract(week from Date_of_Week) as Week,
                                   Channel,
                                   Brand,
                                   Country,
                                   Category,
                                   Sub_Category, Sub_Category_Lv3,
                                   Partnership,
                                   SUM(NMV) as NMV,
                                   sum(Items) as Item,
                                   safe_divide(sum(NMV),sum(Items)) as ASP,
                                   sum(PV) as PV,
                                   safe_divide(sum(Items), sum(PV)) as CR, 
                                   sum(NC) as NC
                                   
                                   from(SELECT 'Lazada' as Channel,
                                   Brand,Country,
                                   case when Cat_Level_1 is null then 'Others' else Cat_Level_1 end as Category,
                                   case when Cat_Level_2 is null then 'Others' else Cat_Level_2 end as Sub_Category,
                                   case when Cat_Level_3 is null then 'Others' else Cat_Level_3 end as Sub_Category_Lv3,
                                   Retail_MP as Partnership,
                                   cast(File_Date as DATE) AS Date_of_Week,
                                   sum(Net_Items) as Items,
                                   SUM(NMV) AS NMV,
                                   sum(PV_App) + sum(PV_Web) as PV,
                                   sum(New_Cust) as NC
                                   FROM SKU_Detail_table
                                   GROUP BY Country, Category,
                                   Sub_Category, Sub_Category_Lv3,
                                   Partnership, Date_of_Week,  Channel, Brand
                                   UNION ALL (  
                                   SELECT 'Shopee' as Channel,
                                   Brand,
                                   Country,
                                   case when Cat_Level_1 is null then 'Others' else Cat_Level_1 end as Category,
                                   case when Cat_Level_2 is null then 'Others' else Cat_Level_2 end as Sub_Category,
                                   case when Cat_Level_3 is null then 'Others' else Cat_Level_3 end as Sub_Category_Lv3,
                                   case when Country = 'TH' then 'MP' else 'Retail' end as Partnership,
                                   # need retail/MP partnership
                                   cast(File_Date as DATE) AS Date_of_Week,
                                   sum(cast(Total_Quantity_Sold as FLOAT64)) as Items,
                                   SUM(Completed_Sales_This_Week_EUR) AS NMV,
                                   sum(Total_Product_Views) as PV,
                                   cast('0' as INT64) as NC
                                   
                                   FROM Shopee_Pdt_Perf_Table
                                   GROUP BY Country, Category,
                                   Sub_Category, Sub_Category_Lv3,
                                   Date_of_Week,
                                   Channel,
                                   Brand,
                                   Partnership))
                                   group by Date_of_Week, Year, Month, Week, Channel,
                                   Brand, Country, Category,
                                   Sub_Category, 
                                   Sub_Category_Lv3,
                                   Partnership")

actuals_subcat_data <- query_exec(get_actuals_subcat_query, project, destination_table = NULL, max_pages = Inf, use_legacy_sql = FALSE)

# target_data <- read_csv("TH_Monthly_Targets.csv")
# shopee_lazada <- read_csv("Shopee_Lazada_Ach.csv")
target_data <- target_data %>%
  mutate(Upload_Date = as.Date(Upload_Date, "%d/%m/%Y")) %>%
  filter(Upload_Date == max(Upload_Date)) %>%
  select(-Upload_Date)

# master table
target_data_ASP_CR <- target_data %>%
  filter(Metric == 'NMV' | Metric == 'Item' | Metric == 'PV') %>%
  spread("Metric", "Target") %>%
  select(1:6, NMV_Target = 'NMV', Item_Target = 'Item', PV_Target = 'PV')

ASP_CR_combined_master <- actuals_data %>%
  left_join(target_data_ASP_CR, by =c("Year", "Country", "Month", "Channel", 
                                      # "Category", 
                                      "Partnership")) %>%
  select("Date_of_Week","Year","Month", "Week", "Channel","Country", "Category_Actuals" = Category.x,
         "Partnership", "NMV", "Item", "ASP", "PV", "CR", "NC", "Category_Targets" = Category.y, "NMV_Target",
         "Item_Target", "PV_Target") %>%
  mutate(Month_Week = paste0(Month, " - Week ", Week))

shopee_lazada_master <- actuals_data %>%
  gather("Metric", "Achieved", 9:14)

combined_Target_Ach_master <- shopee_lazada_master %>%
  left_join(target_data, by = c("Year", "Country", "Month", "Channel", 
                                # "Category", 
                                "Partnership", 
                                "Metric")) %>%
  mutate(pct_Achieved = Achieved/Target) %>%
  select("Date_of_Week","Year","Month", "Week", "Channel","Country", "Category_Actuals" = Category.x,
         "Partnership", "Metric", "Achieved", "Category_Targets" = Category.y, "Target",
         "pct_Achieved") %>%
  mutate(Category_Actuals = case_when(is.na(Category_Actuals) ~ "Others",
                                      TRUE ~ as.character(Category_Actuals)),
         Month_Week = paste0(Month, " - Week ", Week))

actuals_subcat_data_table <- actuals_subcat_data %>%
  gather("Metric", "Achieved", 12:17) %>%
  filter(Metric == 'NMV') %>%
  mutate(Month_Week = paste0(Month, " - Week ", Week))

# Variables for the BigQuery upload portion
destinationProject <- 'unified-welder-172709'
destinationDataset <- 'TH_Marketing_Performance_Dashboard_Targets'
reportName <- 'BQ_Target_Actuals'
subcatreportName <- 'actuals_subcat_data_table'
ASP_CR_reportName <- 'ASP_CR_combined_master_table'

# Check if the table exists, if table exists, then delete the table
tryCatch(delete_table(destinationProject, destinationDataset, reportName),
         error = function(e){
           print(paste0(reportName, " not available for deletion"))
         })

tryCatch(delete_table(destinationProject, destinationDataset, subcatreportName),
         error = function(e){
           print(paste0(reportName, " not available for deletion"))
         })

tryCatch(delete_table(destinationProject, destinationDataset, ASP_CR_reportName),
         error = function(e){
           print(paste0(reportName, " not available for deletion"))
         })


# Upload the table into big query
tryCatch(insert_upload_job(destinationProject, destinationDataset, reportName, combined_Target_Ach_master),
         error = function(e){
           print(paste0(reportName, " failed to upload"))
         })

tryCatch(insert_upload_job(destinationProject, destinationDataset, subcatreportName, actuals_subcat_data_table),
         error = function(e){
           print(paste0(reportName, " failed to upload"))
         })

tryCatch(insert_upload_job(destinationProject, destinationDataset, ASP_CR_reportName, ASP_CR_combined_master),
         error = function(e){
           print(paste0(reportName, " failed to upload"))
         })