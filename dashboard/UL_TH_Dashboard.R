library(tidyverse)
library (bigrquery)
library (lubridate)

# Big Query Setting
project <- "unified-welder-172709"
targets_dataset <- "TH_Marketing_Performance_Dashboard_Targets"

# get targets data by date
get_target_query <- paste0("SELECT
Country, Year, Month,Channel,Category,Partnership,Metric,Target
                           FROM (SELECT
                           Country,Year,Month,Channel,Category,Partnership, 'NMV' AS Metric,
                           ROUND(cast(SUM(NMV) as FLOAT64),2) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6)
                           UNION ALL
                           (SELECT
                           Country,Year,Month,Channel,Category,Partnership, 'Item' AS Metric,
                           ROUND(cast(sum(Item) as FLOAT64),2) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6)
                           UNION ALL
                           (SELECT
                           Country,Year,Month,Channel,Category,Partnership,
                           'ASP' AS Metric,
                           ROUND(cast(SUM(ASP) as FLOAT64),2) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6)
                           UNION ALL
                           (SELECT
                           Country,Year,Month,Channel,Category,Partnership,
                           'PV' AS Metric,ROUND(cast(SUM(PV) as FLOAT64),2) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6)
                           UNION ALL
                           (SELECT
                           Country,Year,Month,Channel,Category,Partnership,
                           'CR' AS Metric,ROUND(SUM(cast(CR as FLOAT64)),2) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6 )
                           UNION ALL
                           (SELECT
                           Country,Year,Month,Channel,Category,Partnership,
                           'NC' AS Metric,ROUND(cast(SUM(NC) as FLOAT64),2) AS Target
                           FROM
                           `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
                           WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                           AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                           GROUP BY 1,2,3,4,5,6)
")
  
target_data <- query_exec(get_target_query, project, destination_table = NULL, max_pages = Inf, use_legacy_sql = FALSE)
  
# get actuals data by date 
get_actuals_query <- paste0("select 
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
                            Channel,
                            Brand,
                            Country,
                            Category,
                            # Sub_Category,
                            Partnership,
                            SUM(NMV) as NMV,
                            sum(Items) as Item,
                            safe_divide(sum(NMV),sum(Items)) as ASP,
                            sum(PV) as PV,
                            safe_divide(sum(Items), sum(PV)) as CR
                            # need NC query
                            from(SELECT 'Lazada' as Channel,
                            Brand,Country,
                            case when Cat_Level_1 is null then 'Others' else Cat_Level_1 end as Category,
                            # case when Cat_Level_2 is null then 'Others' else Cat_Level_2 end as Sub_Category,
                            Retail_MP as Partnership,
                            cast(File_Date as DATE) AS Date_of_Week,
                            sum(Net_Items) as Items,
                            SUM(NMV) AS NMV,
                            sum(PV_App) + sum(PV_Web) as PV
                            FROM `unified-welder-172709.Brand_Partnership_Report_Unilever_SEA.SKU_Detail_*`
                            WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                            AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
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
                            sum(Total_Product_Views) as PV
                            # need NC query
                            FROM `unified-welder-172709.Regional_Official_Seller_Report.Product_Performance_data_*`
                            WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                            AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                            GROUP BY Country, Category,
                            #  Sub_Category,
                            Date_of_Week,
                            Channel,
                            Brand,
                            Partnership))
                            group by Date_of_Week, Year, Month,  Channel,
                            Brand, Country, Category,
                            # Sub_Category,
                            Partnership")
  
actuals_data <- query_exec(get_actuals_query, project, destination_table = NULL, max_pages = Inf, use_legacy_sql = FALSE)

# get actuals data by date 
get_actuals_subcat_query <- paste0("select 
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
                                   Channel,
                                   Brand,
                                   Country,
                                   Category,
                                   Sub_Category,
                                   Partnership,
                                   SUM(NMV) as NMV,
                                   sum(Items) as Item,
                                   safe_divide(sum(NMV),sum(Items)) as ASP,
                                   sum(PV) as PV,
                                   safe_divide(sum(Items), sum(PV)) as CR, 
                                   sum(New_Cust) as NC
                                   # need NC query
                                   from(SELECT 'Lazada' as Channel,
                                   Brand,Country,
                                   case when Cat_Level_1 is null then 'Others' else Cat_Level_1 end as Category,
                                   case when Cat_Level_2 is null then 'Others' else Cat_Level_2 end as Sub_Category,
                                   Retail_MP as Partnership,
                                   cast(File_Date as DATE) AS Date_of_Week,
                                   sum(Net_Items) as Items,
                                   SUM(NMV) AS NMV,
                                   sum(PV_App) + sum(PV_Web) as PV
                                   FROM `unified-welder-172709.Brand_Partnership_Report_Unilever_SEA.SKU_Detail_*`
                                   WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                                   AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                                   GROUP BY Country, Category,
                                   Sub_Category,
                                   Partnership, Date_of_Week,  Channel, Brand
                                   UNION ALL (  
                                   SELECT 'Shopee' as Channel,
                                   Brand,
                                   Country,
                                   case when Cat_Level_1 is null then 'Others' else Cat_Level_1 end as Category,
                                   case when Cat_Level_2 is null then 'Others' else Cat_Level_2 end as Sub_Category,
                                   case when Country = 'TH' then 'MP' else 'Retail' end as Partnership,
                                   # need retail/MP partnership
                                   cast(File_Date as DATE) AS Date_of_Week,
                                   sum(cast(Total_Quantity_Sold as FLOAT64)) as Items,
                                   SUM(Completed_Sales_This_Week_EUR) AS NMV,
                                   sum(Total_Product_Views) as PV,
                                   cast('0' as INT64) as NC
                                   # need NC query
                                   FROM `unified-welder-172709.Regional_Official_Seller_Report.Product_Performance_data_*`
                                   WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
                                   AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
                                   GROUP BY Country, Category,
                                   Sub_Category,
                                   Date_of_Week,
                                   Channel,
                                   Brand,
                                   Partnership))
                                   group by Date_of_Week, Year, Month,  Channel,
                                   Brand, Country, Category,
                                   Sub_Category,
                                   Partnership")

actuals_subcat_data <- query_exec(get_actuals_subcat_query, project, destination_table = NULL, max_pages = Inf, use_legacy_sql = FALSE)

# target_data <- read_csv("TH_Monthly_Targets.csv")
# shopee_lazada <- read_csv("Shopee_Lazada_Ach.csv")

# master table
#target_data_master <- target_data %>%
#  gather("Metric", "Target", 8:13) 

shopee_lazada_master <- actuals_data %>%
  gather("Metric", "Achieved", 9:13)

combined_Target_Ach_master <- shopee_lazada_master %>%
  left_join(target_data, by = c("Year", "Country", "Month", "Channel", 
                                      # "Category", 
                                       "Partnership", 
                                       "Metric")) %>%
  mutate(pct_Achieved = Achieved/Target) %>%
  select("Date_of_Week","Year","Month","Channel","Brand","Country", "Category_Actuals" = Category.x,
         "Partnership", "Metric", "Achieved", "Category_Targets" = Category.y, "Target",
         "pct_Achieved") %>%
  mutate(Category_Actuals = case_when(is.na(Category_Actuals) ~ "Others",
         TRUE ~ as.character(Category_Actuals)))

actuals_subcat_data_table <- actuals_subcat_data %>%
  gather("Metric", "Achieved", 10:14) %>%
  filter(Metric == 'NMV')

# Variables for the BigQuery upload portion
destinationProject <- 'unified-welder-172709'
destinationDataset <- 'TH_Marketing_Performance_Dashboard_Targets'
reportName <- 'BQ_Target_Actuals'
subcatreportName <- 'actuals_subcat_data_table'

# Check if the table exists, if table exists, then delete the table
tryCatch(delete_table(destinationProject, destinationDataset, reportName),
         error = function(e){
           print(paste0(reportName, " not available for deletion"))
         })

tryCatch(delete_table(destinationProject, destinationDataset, subcatreportName),
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

