library(bigrquery)
library(tidyverse)


dataset <- bq_project_datasets("airnz-ga-bigquery")


# list tables in a project

dataset_register <- bq_dataset("airnz-ga-bigquery", "Master_Reporting_Data")

table_list <- bq_dataset_tables(dataset_register)

dataset_table_list <- list()

for (i in 1:length(table_list)) {
  k <- table_list[[i]]$table
  dataset_table_list <- append(dataset_table_list, k)
}

dataset_table_list <- tibble(dataset_table_list) %>% 
  rename(table_name = dataset_table_list)

# ---------------------------------------------

dataset_list <- list()

for (i in 1:length(dataset)) {
  s <- dataset[[i]]$dataset
  dataset_list <- append(dataset_list, s)
}

datasets <- unlist(dataset_list)
expired_dfs <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("project_id", "dataset_id", "table_id", "created_date")
colnames(expired_dfs) <- x


project <- "airnz-ga-bigquery"


for (i in 1:length(dataset)) {
get_data_query <- paste0("SELECT 
project_id,	dataset_id,	table_id, TIMESTAMP_MILLIS(creation_time	) created_date
FROM `", datasets[i], ".__TABLES__` 
where date(TIMESTAMP_MILLIS(creation_time	)) < date_sub(date(current_timestamp()), interval 3 year )")

expired_df_output <- bq_table_download(bq_project_query(project,
                                                     get_data_query,
                                                     use_legacy_sql = FALSE))

expired_dfs <- union_all(expired_dfs, expired_df_output)
}

write_csv(expired_dfs, "expired_dfs.csv")



#### delete the tables that are more than 3 years old - need testing #####

for (i in 1:length(expired_dfs$project_id)) {
tryCatch(
  bq_table_delete(
    bq_table(project, expired_dfs$dataset_id[i], expired_dfs$dataset_id[i])
  ),
  error = function(e) {
    print(paste0(expired_dfs$dataset_id[i], " not available for deletion"))
  }
)
}
