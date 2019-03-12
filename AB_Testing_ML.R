# https://www.business-science.io/business/2019/03/11/ab-testing-machine-learning.html
# Core packages
library(tidyverse)
library(tidyquant)

# Modeling packages
library(parsnip)
library(recipes)
library(rsample)
library(yardstick)
library(broom)
library(stats)

# Connector packages
library(rpart)
library(rpart.plot)
library(xgboost)
library(plotly)

#### Import data #######
control_tbl <- read_csv("control_data.csv")
experiment_tbl <- read_csv("experiment_data.csv")

control_tbl %>% head(5)
control_tbl %>% glimpse()
experiment_tbl %>% glimpse()

#### check for missing data #######
control_tbl %>%
  map_df(~ sum(is.na(.))) %>%
  gather(key = "feature", value = "missing_count") %>%
  arrange(desc(missing_count))

experiment_tbl %>% 
  map_df(~ sum(is.na(.))) %>%
  gather(key = "feature", value = "missing_count") %>%
  arrange(desc(missing_count))

control_tbl %>%
  filter(is.na(Enrollments))

### format data #######
set.seed(123)
data_formatted_tbl <- control_tbl %>%
  
  # Combine with Experiment data
  bind_rows(experiment_tbl, .id = "Experiment") %>%
  mutate(Experiment = as.numeric(Experiment) - 1) %>%
  
  # Add row id
  mutate(row_id = row_number()) %>%
  
  # Create a Day of Week feature
  mutate(DOW = str_sub(Date, start = 1, end = 3) %>% 
           factor(levels = c("Sun", "Mon", "Tue", "Wed", 
                             "Thu", "Fri", "Sat"))
  ) %>%
  select(-Date, -Payments) %>%
  
  # Remove missing data
  filter(!is.na(Enrollments)) %>%
  
  # Shuffle the data (note that set.seed is used to make reproducible)
  sample_frac(size = 1) %>%
  
  # Reorganize columns
  select(row_id, Enrollments, Experiment, everything())

data_formatted_tbl %>% glimpse()


set.seed(123)
split_obj <- data_formatted_tbl %>%
  initial_split(prop = 0.8, strata = "Experiment")

train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)
train_tbl %>% glimpse()
test_tbl %>% glimpse()

#### Implement ML algorithms #####
model_01_lm <- linear_reg("regression") %>%
  set_engine("lm") %>%
  fit(Enrollments ~ ., data = train_tbl %>% select(-row_id))

# knitr::kable() used for pretty tables
model_01_lm %>%
  predict(new_data = test_tbl) %>%
  bind_cols(test_tbl %>% select(Enrollments)) %>%
  metrics(truth = Enrollments, estimate = .pred) %>%
  knitr::kable()


model_01_lm %>%
  # Format Data
  predict(test_tbl) %>%
  bind_cols(test_tbl %>% select(Enrollments)) %>%
  mutate(observation = row_number() %>% as.character()) %>%
  gather(key = "key", value = "value", -observation, factor_key = TRUE) %>%
  
  # Visualize
  ggplot(aes(x = observation, y = value, color = key)) +
  geom_point() +
  expand_limits(y = 0) +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Enrollments: Prediction vs Actual",
       subtitle = "Model 01: Linear Regression (Baseline)")

#### interactive plot #####

lm_plot <- model_01_lm %>%
  # Format Data
  predict(test_tbl) %>%
  bind_cols(test_tbl %>% select(Enrollments)) %>%
  mutate(observation = row_number() %>% as.character()) %>%
  gather(key = "key", value = "value", -observation, factor_key = TRUE) %>%
  
  # Visualize
  ggplot(aes(x = observation, y = value, color = key)) +
  geom_point() +
  expand_limits(y = 0) +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Enrollments: Prediction vs Actual",
       subtitle = "Model 01: Linear Regression (Baseline)")

plotly::ggplotly(lm_plot)


#### inspect the regression model ######
linear_regression_model_terms_tbl <- as.list(model_01_lm$fit) %>%
  tidy() %>%
  arrange(p.value) %>%
  mutate(term = as_factor(term) %>% fct_rev()) 

# knitr::kable() used for pretty tables
linear_regression_model_terms_tbl %>% knitr::kable()

linear_regression_model_terms_tbl %>%
  ggplot(aes(x = p.value, y = term)) +
  geom_point(color = "#2C3E50") +
  geom_vline(xintercept = 0.05, linetype = 2, color = "red") +
  theme_tq() +
  labs(title = "Feature Importance",
       subtitle = "Model 01: Linear Regression (Baseline)")


### make a simplified reporting function ####
calc_metrics <- function(model, new_data) {
  model %>%
    predict(new_data = new_data) %>%
    bind_cols(new_data %>% select(Enrollments)) %>%
    metrics(truth = Enrollments, 
            estimate = .pred)
}

### make a simplified prediction function ####
plot_predictions <- function(model, new_data) {
  
  g <- predict(model, new_data) %>%
    bind_cols(new_data %>% select(Enrollments)) %>%
    mutate(observation = row_number() %>% as.character()) %>%
    gather(key = "key", value = "value", -observation, factor_key = TRUE) %>%
    
    # Visualize
    ggplot(aes(x = observation, y = value, color = key)) +
    geom_point() +
    expand_limits(y = 0) +
    theme_tq() +
    scale_color_tq()
  
  return(g)
}

### create a decision tree ####
model_02_decision_tree <- decision_tree(
  mode = "regression",
  cost_complexity = 0.001, 
  tree_depth = 5, 
  min_n = 4) %>%
  set_engine("rpart") %>%
  fit(Enrollments ~ ., data = train_tbl %>% select(-row_id))

# knitr::kable() used for pretty tables
model_02_decision_tree %>% 
  calc_metrics(test_tbl) %>%
  knitr::kable()

model_02_decision_tree %>% 
  plot_predictions(test_tbl) +
  labs(title = "Enrollments: Prediction vs Actual",
       subtitle = "Model 02: Decision Tree")


model_02_decision_tree$fit %>%
  rpart.plot(
    roundint = FALSE, 
    cex = 0.8, 
    fallen.leaves = TRUE,
    extra = 101, 
    main = "Model 02: Decision Tree")


set.seed(123)
model_03_xgboost <- boost_tree(
  mode = "regression",
  mtry = 100, 
  trees = 1000, 
  min_n = 8, 
  tree_depth = 6, 
  learn_rate = 0.2, 
  loss_reduction = 0.01, 
  sample_size = 1) %>%
  set_engine("xgboost") %>%
  fit(Enrollments ~ ., data = train_tbl %>% select(-row_id))

# knitr::kable() used for pretty tables
model_03_xgboost %>% 
  calc_metrics(test_tbl) %>%
  knitr::kable()

model_03_xgboost %>% plot_predictions(test_tbl) +
  labs(title = "Enrollments: Prediction vs Actual",
       subtitle = "Model 02: Decision Tree")


xgboost_feature_importance_tbl <- model_03_xgboost$fit %>%
  xgb.importance(model = .) %>%
  as_tibble() %>%
  mutate(Feature = as_factor(Feature) %>% fct_rev())

xgboost_feature_importance_tbl %>% knitr::kable()


xgboost_feature_importance_tbl %>%
  ggplot(aes(x = Gain, y = Feature)) +
  geom_point(color = "#2C3E50") +
  geom_label(aes(label = scales::percent(Gain)), 
             hjust = "inward", color = "#2C3E50") +
  expand_limits(x = 0) +
  theme_tq() +
  labs(title = "XGBoost Feature Importance") 




