# http://uc-r.github.io/random_forests

library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform
library(AmesHousing)

# Create training (70%) and test (30%) sets for the AmesHousing::make_ames() data.
# Use set.seed for reproducibility

set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

# The basic algorithm for a regression random forest can be generalized to the following:
#1.  Given training data set
#2.  Select number of trees to build (ntrees)
#3.  for i = 1 to ntrees do
#4.  |  Generate a bootstrap sample of the original data
#5.  |  Grow a regression tree to the bootstrapped data
#6.  |  for each split do
#7.  |  | Select m variables at random from all p variables
#8.  |  | Pick the best variable/split-point among the m
#9.  |  | Split the node into two child nodes
#10. |  end
#11. | Use typical tree model stopping criteria to determine when a tree is complete (but do not prune)
#12. end

# for reproduciblity
set.seed(123)

# default RF model
m1 <- randomForest(
  formula = Sale_Price ~ .,
  data    = ames_train
)

m1

# Plotting the model will illustrate the error rate as we average across more trees 
# and shows that our error rate stabalizes with around 100 trees but continues to 
# decrease slowly until around 300 or so trees.

plot(m1)

# The plotted error rate above is based on the OOB sample error 
# and can be accessed directly at m1$mse. Thus, we can find which number of trees providing 
# the lowest error rate

# number of trees with lowest MSE
which.min(m1$mse)


# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])


# randomForest also allows us to use a validation set to measure predictive accuracy 
# if we did not want to use the OOB samples. Here we split our training set further to 
# create a training and validation set. We then supply the validation data in the xtest 
# and ytest arguments.

# create training and validation data 
set.seed(123)
valid_split <- initial_split(ames_train, .8)

# training data
ames_train_v2 <- analysis(valid_split)

# validation data
ames_valid <- assessment(valid_split)
x_test <- ames_valid[setdiff(names(ames_valid), "Sale_Price")]
y_test <- ames_valid$Sale_Price

rf_oob_comp <- randomForest(
  formula = Sale_Price ~ .,
  data    = ames_train_v2,
  xtest   = x_test,
  ytest   = y_test
)

# extract OOB & validation errors
oob <- sqrt(rf_oob_comp$mse)
validation <- sqrt(rf_oob_comp$test$mse)

# compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Number of trees")

# Initial tuning with randomForest
# If we are interested with just starting out and tuning the 
# mtry parameter we can use randomForest::tuneRF for a quick and easy tuning assessment. 
# tuneRf will start at a value of mtry that you supply and increase by a certain step factor 
# until the OOB error stops improving be a specified amount.

# names of features
features <- setdiff(names(ames_train), "Sale_Price")

set.seed(123)

m2 <- tuneRF(
  x          = ames_train[features],
  y          = ames_train$Sale_Price,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)

# Full grid search with ranger
# To perform a larger grid search across several hyperparameters we’ll need to create 
# a grid and loop through each hyperparameter combination and evaluate the model. 
# Unfortunately, this is where randomForest becomes quite inefficient since it does not scale well. 
# Instead, we can use ranger which is a C++ implementation of 
# Brieman’s random forest algorithm and, as the following illustrates, 
# is over 6 times faster than randomForest.

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(20, 30, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

# total number of combinations
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = Sale_Price ~ ., 
    data            = ames_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)


# Although, random forests typically perform quite well with categorical variables in their original columnar form, 
# it is worth checking to see if alternative encodings can increase performance. 
# For example, the following one-hot encodes our categorical variables which produces 353 predictor variables 
# versus the 80 we were using above. We adjust our mtry parameter to search from 50-200
# random predictor variables at each split and re-perform our grid search. 
# The results suggest that one-hot encoding does not improve performance.

# one-hot encode our categorical variables
one_hot <- dummyVars(~ ., ames_train, fullRank = FALSE)
ames_train_hot <- predict(one_hot, ames_train) %>% as.data.frame()

# make ranger compatible names
names(ames_train_hot) <- make.names(names(ames_train_hot), allow_ = FALSE)

# hyperparameter grid search --> same as above but with increased mtry values
hyper_grid_2 <- expand.grid(
  mtry       = seq(50, 200, by = 25),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE  = 0
)

# perform grid search
for(i in 1:nrow(hyper_grid_2)) {
  
  # train model
  model <- ranger(
    formula         = Sale.Price ~ ., 
    data            = ames_train_hot, 
    num.trees       = 500,
    mtry            = hyper_grid_2$mtry[i],
    min.node.size   = hyper_grid_2$node_size[i],
    sample.fraction = hyper_grid_2$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid_2$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid_2 %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

# Currently, the best random forest model we have found retains columnar categorical variables 
# and uses mtry = 24, terminal node size of 5 observations, and a sample size of 80%. 
# Lets repeat this model to get a better expectation of our error rate.

OOB_RMSE <- vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula         = Sale_Price ~ ., 
    data            = ames_train, 
    num.trees       = 500,
    mtry            = 24,
    min.node.size   = 5,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks = 20)

# we accumulate the reduction in MSE for each variable across all the trees and the 
# variable with the greatest accumulated impact is considered the more important, or impactful. 
# We see that Overall_Qual has the greatest impact in reducing MSE across our trees, 
# followed by Gr_Liv_Area, Garage_Cars, etc.

optimal_ranger$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 25 important variables")

# full grid search with h2o
# Moreover, h2o allows for different optimal search paths in our grid search. 
# This allows us to be more efficient in tuning our models.

# start up h2o 
h2o.no_progress()
h2o.init(max_mem_size = "5g")

# create feature names
y <- "Sale_Price"
x <- setdiff(names(ames_train), y)

# turn training set into h2o object
train.h2o <- as.h2o(ames_train)

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 100),
  mtries      = seq(20, 30, by = 2),
  sample_rate = c(.55, .632, .70, .80)
)

# build grid search 
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 150),
  mtries      = seq(15, 35, by = 10),
  max_depth   = seq(20, 40, by = 5),
  min_rows    = seq(1, 5, by = 2),
  nbins       = seq(10, 30, by = 5),
  sample_rate = c(.55, .632, .75)
)

# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_runtime_secs = 30*60
)

# build grid search 
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid2",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)

# collect the results and sort by our model performance metric of choice
grid_perf2 <- h2o.getGrid(
  grid_id = "rf_grid2", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf2)

# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf2@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let’s evaluate the model performance on a test set
ames_test.h2o <- as.h2o(ames_test)
best_model_perf <- h2o.performance(model = best_model, newdata = ames_test.h2o)

# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt()


# randomForest
pred_randomForest <- predict(ames_randomForest, ames_test)
head(pred_randomForest)

# ranger
pred_ranger <- predict(ames_ranger, ames_test)
head(pred_ranger$predictions)

# h2o
pred_h2o <- predict(best_model, ames_test.h2o)
head(pred_h2o)





