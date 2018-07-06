library(tidyverse)
library(bigrquery)
library(lubridate)

# Big Query Setting ----------------------------------------------------
project <- "unified-welder-172709"
targets_dataset <- "TH_Marketing_Performance_Dashboard_Targets"

# get targets data by date ---------------------------------------------
# for use with cat and cat summary dashboards
get_target_query <- paste0(
   "SELECT
   Upload_Date, Country,
   Year, Month,Channel,Category,
   Partnership,Metric,Target
   FROM (SELECT
   Upload_Date,Country,Year,
   Month,Channel,Category,
   Partnership, 'NMV' AS Metric,
   cast(SUM(NMV) as FLOAT64) AS Target
   FROM
   `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
   WHERE
   _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
   AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) AND
   Upload_Date > '2018-06-01'
   GROUP BY 1,2,3,4,5,6,7,8)
   UNION ALL (SELECT
   Upload_Date, Country,Year,Month,
   Channel,Category,Partnership, 'Item' AS Metric,
   ROUND(cast(sum(Item) as FLOAT64),2) AS Target
   FROM `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
   WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
   AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) AND Upload_Date > '2018-06-01'
   GROUP BY 1,2,3,4,5,6,7,8)
   UNION ALL
   (SELECT
   Upload_Date, Country,Year,Month,Channel,Category,Partnership,
   'ASP' AS Metric,
   cast(SUM(ASP) as FLOAT64) AS Target
   FROM
   `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
   WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
   AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) AND Upload_Date > '2018-06-01'
   GROUP BY 1,2,3,4,5,6,7,8)
   UNION ALL (SELECT
   Upload_Date, Country,Year,Month,Channel,Category,Partnership,
   'PV' AS Metric,ROUND(cast(SUM(PV) as FLOAT64),2) AS Target
   FROM `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
   WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
   AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) AND Upload_Date > '2018-06-01'
   GROUP BY 1,2,3,4,5,6,7,8)
   UNION ALL (SELECT
   Upload_Date, Country,Year,Month,Channel,Category,Partnership,
   'CR' AS Metric,cast(CR as FLOAT64) AS Target
   FROM `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
   WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
   AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) AND Upload_Date > '2018-06-01'
   GROUP BY 1,2,3,4,5,6,7,8,9 )
   UNION ALL (SELECT
   Upload_Date, Country,Year,Month,Channel,Category,Partnership,
   'NC' AS Metric, cast(SUM(NC) as FLOAT64) AS Target
   FROM
   `unified-welder-172709.TH_Marketing_Performance_Dashboard_Targets.Monthly_Targets_*`
   WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
   AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) AND Upload_Date > '2018-06-01'
   GROUP BY 1,2,3,4,5,6,7,8)"
  )

target_data <-
  bq_table_download(bq_project_query(project, get_target_query))

# get actuals data for shopee and lazada by date ---------------------------------------------------
# for use with cat and cat summary dashboards,
get_actuals_query <-
  paste0(
    "WITH SKU_Detail_table AS (SELECT max(upload_date) as Ud,
    File_Date,	Partner,	Country,	Cat_Level_1,
    Cat_Level_2,	Cat_Level_3,	Cat_Level_4,	Cat_Level_5,
    Cat_Level_6,	Product_Name,	Brand,	SKU,	Brand_SKU,
    Retail_MP,	Seller_Name,	Seller_BOB_ID,
    Lazada_Partner,	List_Price,	Current_Lazada_Price,	Competitor_Price,
    Competitive_Flag,	Competitor_URL,	Top_1_L7D,
    Top_2_L7D,	Top_3_L7D,	Top_4_L7D,	Top_5_L7D,
    Stock_Level,	Inventory_Coverage_in_Days,
    Date_Period,	Customers,	Lazada_Own_Media,
    Organic_Traffic,	Paid_Media,	Referrals,
    Male,	Female,	Age_0_18,	Age_19_25,
    Age_26_35,	Age_35_older,	Price,	Item_Order,
    desktop,	iosApp,	androidApp,	mobile,	tablet,	COD,
    E_Payment,	Bank_Transfer,	Credit_Card,
    Urban,	Rural,	NMV,	Net_Items,	Net_Orders,
    PV_Web,	CR_Web,	PV_App,	CR_App,	New_Cust
    FROM `unified-welder-172709.Brand_Partnership_Report_Unilever_SEA.SKU_Detail_*`
    WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
    AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31')) AND
    Date_Period = 'L7D' group by 2,3,4,5,6,7,8,9,10,
    11,12,13,14,15,16,17,18,19,20,21,22,
    23,24,25,26,27,28,29,30,31,32,33,34,
    35,36,37,38,39,40,41,42,43,44,45,46,47,
    48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63),
    Shopee_Pdt_Perf_Table as (SELECT max(upload_date) as Ud,
    File_Date,	Partner,	Country,	Start_Date,	End_Date,	Retailer,
    Item_Name,	Category,	Sub_Category,	Total_Quantity_Sold,
    Stock_Available,	Days_before_OOS,	Restock_Action_Needed,
    Suggested_Replenishment_1_week_coverage,	No_of_Unique_Buyers_Previous_Week,
    No_of_Unique_Buyers_This_Week,	Completed_Orders_Previous_Week,
    Completed_Orders_This_Week,	Completed_Sales_Previous_Week_Local_Currency,
    Completed_Sales_This_Week_Local_Currency,
    Original_Listing_Price_Local_Currency,
    Average_Listing_Price_Local_Currency,	Total_Product_Views,
    Cat_Level_1,	Cat_Level_2, Cat_Level_3,
    No_of_Unique_Buyers_WoW,	Completed_Orders_WoW,	Completed_Sales_WoW,
    Average_Discount_Level, Conversion_Rate,	Product_Return_Rate,
    Local_Currency,	EUR_Forex_Rate,	USD_Forex_Rate,
    Completed_Sales_Previous_Week_EUR, Completed_Sales_Previous_Week_USD,
    Completed_Sales_This_Week_EUR,	Completed_Sales_This_Week_USD,
    Original_Listing_Price_EUR,
    Original_Listing_Price_USD,	Average_Listing_Price_EUR,
    Average_Listing_Price_USD,	Brand
    FROM `unified-welder-172709.Regional_Official_Seller_Report.Product_Performance_data_*`
    WHERE  _TABLE_SUFFIX BETWEEN FORMAT_DATE('%Y%m%d', DATE('2016-01-01'))
    AND FORMAT_DATE('%Y%m%d', DATE('2019-12-31'))
    group by 2,3,4,5,6,7,8,9,10,11, 12,13,14,15,
    16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
    31,32,33,34,35,36,37,38,39,40,41,42,43,44,45),
    forex_table as (select * from `unified-welder-172709.Lookup_Tables.Forex_Rates` where From_Currency = 'THB'
    and To_Currency = 'USD')
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
    Sub_Category,
    Partnership,
    SUM(NMV) as NMV,
    sum(Items) as Item,
    safe_divide(sum(NMV),sum(Items)) as ASP,
    sum(PV) as PV,
    safe_divide(sum(Items), sum(PV)) as CR,
    sum(NC) as NC
    from(SELECT 'Lazada' as Channel,
    Brand,Country,
    case when Cat_Level_1 is null then 'Others'
    else Cat_Level_1 end as Category,
    case when Cat_Level_2 is null then 'Others'
    else Cat_Level_2 end as Sub_Category,
    Retail_MP as Partnership,
    cast(File_Date as DATE) AS Date_of_Week,
    sum(Net_Items) as Items,
    SUM(NMV) * (select 1/rate from forex_table) as NMV,
    sum(PV_App) + sum(PV_Web) as PV,
    sum(New_Cust) as NC
    FROM SKU_Detail_table
    GROUP BY Country, Category,
    Sub_Category,
    Partnership, Date_of_Week,  Channel, Brand
    UNION ALL (
    SELECT 'Shopee' as Channel,
    Brand,
    Country,
    case when Cat_Level_1 is null then 'Others'
    else Cat_Level_1 end as Category,
    case when Cat_Level_2 is null then 'Others'
    else Cat_Level_2 end as Sub_Category,
    case when Country = 'TH' then 'MP' else 'Retail' end as Partnership,
    # need retail/MP partnership
    cast(File_Date as DATE) AS Date_of_Week,
    sum(cast(Total_Quantity_Sold as FLOAT64)) as Items,
    SUM(Completed_Sales_This_Week_Local_Currency) AS NMV,
    sum(Total_Product_Views) as PV,
    cast('0' as INT64) as NC
    FROM Shopee_Pdt_Perf_Table
    GROUP BY Country, Category,
    Sub_Category,
    Date_of_Week,
    Channel,
    Brand,
    Partnership))
    group by Date_of_Week, Year, Month, Week, Channel,
    Brand,
    Country, Category,
    Sub_Category,
    Partnership"
)

actuals_data <-
  bq_table_download(bq_project_query(project, get_actuals_query))


# get data for use in campaign commercial performance dashboard -------------------------
campaign_comm_perf_pivotquery <- paste0(
  "SELECT *
  FROM (
  SELECT
  CAST(date AS date) AS date,
  Brand,
  Cat_Level_1,
  Cat_Level_2,
  Cat_Level_3,
  DATE_SUB(CAST(date AS date), INTERVAL 7 DAY) AS seven_days_ago,
  DATE_SUB(CAST(date AS date), INTERVAL 1 MONTH) AS previous_month,
  SUM(NMV_Local_Currency) AS NMV_current,
  SUM(Item) AS Item_current,
  SUM(PV) AS PV_current,
  safe_divide(SUM(Item),
  SUM(PV)) AS CR_current,
  safe_divide(SUM(NMV_Local_Currency),
  SUM(Item)) AS ASP_current
  FROM
  `unified-welder-172709.Daily_Lazada_Report.Daily_Lazada_Sales_*`
  WHERE
  _table_suffix BETWEEN FORMAT_DATE('%Y%m%d', DATE('2018-01-01'))
  AND FORMAT_DATE('%Y%m%d', DATE('2018-12-31'))
  GROUP BY
  1,
  2,
  3,4,5,6,7) AS A
  LEFT JOIN (
  SELECT
  #  FORMAT_DATE('%b', CAST(date AS date)) AS month_sevendays,
  CAST(date AS date) AS date_sevendays,
  Brand as Brand_B, Cat_Level_1 as Cat_Level_1_B,
  Cat_Level_2 as Cat_Level_2_B,	Cat_Level_3 as Cat_Level_3_B,
  SUM(NMV_Local_Currency) AS NMV_sevendays,
  SUM(Item) AS Item_sevendays,
  SUM(PV) AS PV_sevendays,
  safe_divide(SUM(Item),
  SUM(PV)) AS CR_sevendays,
  safe_divide(SUM(NMV_Local_Currency),
  SUM(Item)) AS ASP_sevendays
  FROM
  `unified-welder-172709.Daily_Lazada_Report.Daily_Lazada_Sales_*`
  WHERE
  _table_suffix BETWEEN FORMAT_DATE('%Y%m%d', DATE('2018-01-01'))
  AND FORMAT_DATE('%Y%m%d', DATE('2018-12-31'))
  GROUP BY
  1,2,3,4,5) AS B
  ON
  A.seven_days_ago = B.date_sevendays and
  A.Brand = B.Brand_B and A.Cat_Level_1 = B.Cat_Level_1_B and A.Cat_Level_2 = B.Cat_Level_2_B
  and A.Cat_Level_3 = B.Cat_Level_3_B
  LEFT JOIN (
  SELECT
  #  FORMAT_DATE('%b', CAST(date AS date)) AS month_previousmonth,
  CAST(date AS date) AS date_previousmonth,
  Brand as Brand_C, Cat_Level_1 as Cat_Level_1_C,
  Cat_Level_2 as Cat_Level_2_C,	Cat_Level_3 as Cat_Level_3_C,
  SUM(NMV_Local_Currency) AS NMV_previousmonth,
  SUM(Item) AS Item_previousmonth,
  SUM(PV) AS PV_previousmonth,
  safe_divide(SUM(Item),
  SUM(PV)) AS CR_previousmonth,
  safe_divide(SUM(NMV_Local_Currency),
  SUM(Item)) AS ASP_previousmonth
  FROM
  `unified-welder-172709.Daily_Lazada_Report.Daily_Lazada_Sales_*`
  WHERE
  _table_suffix BETWEEN FORMAT_DATE('%Y%m%d', DATE('2018-01-01'))
  AND FORMAT_DATE('%Y%m%d', DATE('2018-12-31'))
  GROUP BY
  1,2,3,4,5) AS C
  ON
  A.previous_month = C.date_previousmonth and
  A.Brand = C.Brand_C and A.Cat_Level_1 = C.Cat_Level_1_C
  and A.Cat_Level_2 = C.Cat_Level_2_C
  and A.Cat_Level_3 = C.Cat_Level_3_C"
)

# unpivot metrics from campaign commercial performance dataset ------------------------
campaigncommercialperf_bqdata <-
  bq_table_download(bq_project_query(project, campaign_comm_perf_pivotquery))
campaign_commercial_perf_table <- campaigncommercialperf_bqdata %>%
  select(1:7, 13, 23, 8:12, 18:22, 28:32) %>%
  gather(metric, value, 10:24) %>%
  separate(metric, c("metric", "period"), "_") %>%
  spread(period, value) %>%
  select(1:10, present = "current", 12:13)

# get target data only for the latest upload dates ------------------------------------
target_data <- target_data %>%
  mutate(Upload_Date = as.Date(Upload_Date, "%d/%m/%Y")) %>%
  filter(Upload_Date == max(Upload_Date)) %>%
  select(-Upload_Date)

# create master table of targets and actual performance ----------------------------------------
target_data_ASP_CR <- target_data %>%
  filter(Metric == "NMV" | Metric == "Item" | Metric == "PV") %>%
  spread("Metric", "Target") %>%
  select(
    1:6,
    Sub_Category = "Category",
    NMV_Target = "NMV",
    Item_Target = "Item",
    PV_Target = "PV"
  )

# create separate master table for ASP and CR --------------------------------------------------
asp_cr_combined_master <- actuals_data %>%
  left_join(
    target_data_ASP_CR,
    by = c(
      "Year",
      "Country",
      "Month",
      "Channel",
      "Sub_Category",
      "Partnership"
    )
  ) %>%
  select(
    "Date_of_Week",
    "Year",
    "Month",
    "Week",
    "Channel",
    "Country",
    "Partnership",
    "NMV",
    "Item",
    "ASP",
    "PV",
    "CR",
    "NC",
    "Category",
    "Sub_Category",
    "NMV_Target",
    "Item_Target",
    "PV_Target"
  ) %>%
  mutate(Month_Week = paste0(Month, " - Week ", Week))

# unpivot data from actual results table --------------------------------------------
shopee_lazada_master <- actuals_data %>%
  gather("Metric", "Achieved", 11:16)

# create master table for use in Summary_Targets_Table: check this join query after target table uploaded-------------------------------------------
combined_Target_Ach_master <- shopee_lazada_master %>%
  left_join(
    target_data,
    by = c(
      "Year",
      "Country",
      "Month",
      "Channel",
      "Sub_Category" = "Category",
      "Partnership",
      "Metric"
    )
  ) %>%
  mutate(pct_Achieved = Achieved / Target) %>%
  select(
    "Date_of_Week",
    "Year",
    "Month",
    "Week",
    "Channel",
    "Country",
    "Partnership",
    "Metric",
    "Achieved",
    "Category",
    "Sub_Category",
    "Target",
    "pct_Achieved"
  ) %>%
  mutate(
    Category = case_when(is.na(Category) ~ "Others",
                         TRUE ~ as.character(Category)),
    Month_Week = paste0(Month, " - Week ", Week)
  )


# Variables for the BigQuery upload portion ----------------------------------------------
destinationproject <- "unified-welder-172709"
destinationdataset <- "TH_Marketing_Performance_Dashboard_Targets"
reportname <- "BQ_Target_Actuals"
asp_cr_reportname <- "asp_cr_combined_master_table"
campaign_commercial_reportname <- "campaign_commercial_perf_table"

# Check if the table exists, if table exists, then delete the table ------------------------
tryCatch(
  bq_table_delete(bq_table(
    destinationproject, destinationdataset, reportname
  )),
  error = function(e) {
    print(paste0(reportname, " not available for deletion"))
  }
)


tryCatch(
  bq_table_delete(
    bq_table(destinationproject, destinationdataset, asp_cr_reportname)
  ),
  error = function(e) {
    print(paste0(asp_cr_reportname, " not available for deletion"))
  }
)

tryCatch(
  bq_table_delete(
    bq_table(project, targets_dataset, campaign_commercial_reportname)
  ),
  error = function(e) {
    print(paste0(campaign_commercial_reportname, " not available for deletion"))
  }
)



# Upload the table into big query -----------------------------
tryCatch(
  insert_upload_job(
    destinationproject,
    destinationdataset,
    reportname,
    combined_Target_Ach_master
  ),
  error = function(e) {
    print(paste0(combined_Target_Ach_master, " failed to upload"))
  }
)


tryCatch(
  insert_upload_job(
    destinationproject,
    destinationdataset,
    asp_cr_reportname,
    asp_cr_combined_master
  ),
  error = function(e) {
    print(paste0(asp_cr_combined_master, " failed to upload"))
  }
)


tryCatch(
  insert_upload_job(
    destinationproject,
    destinationdataset,
    campaign_commercial_reportname,
    campaign_commercial_perf_table
  ),
  error = function(e) {
    print(paste0(campaign_commercial_perf_table, " failed to upload"))
  }
)
