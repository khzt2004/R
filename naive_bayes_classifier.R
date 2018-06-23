# http://uc-r.github.io/naive_bayes

library(rsample)  # data splitting 
library(dplyr)    # data transformation
library(ggplot2)  # data visualization
library(caret)    # implementing with caret
library(h2o)      # implementing with h2o

# convert some numeric variables to factors
attrition <- attrition %>%
  mutate(
    JobLevel = factor(JobLevel),
    StockOptionLevel = factor(StockOptionLevel),
    TrainingTimesLastYear = factor(TrainingTimesLastYear)
  )

# Create training (70%) and test (30%) sets for the attrition data.
# Use set.seed for reproducibility
set.seed(123)
split <- initial_split(attrition, prop = .7, strata = "Attrition")
train <- training(split)
test  <- testing(split)

# distribution of Attrition rates across train & test set
table(train$Attrition) %>% prop.table()

table(test$Attrition) %>% prop.table()

# With naïve Bayes, we assume that the predictor variables are conditionally independent 
# of one another given the response value. This is an extremely strong assumption. 
# We can see quickly that our attrition data violates this 
# as we have several moderately to strongly correlated variables.
train %>%
  filter(Attrition == "Yes") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

# However, when including continuous predictor variables often an assumption of normality 
# is made so that we can use the probability from the variable’s probability density function. 
# If we pick a handful of our numeric features we quickly see 
# assumption of normality is not always fair.

train %>% 
  select(Age, DailyRate, DistanceFromHome, HourlyRate, MonthlyIncome, MonthlyRate) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")

# The greatest weakness of the naïve Bayes classifier is that it relies on an often-faulty assumption 
# of equally important and independent features which results in biased posterior probabilities. 
# Although this assumption is rarely met, in practice, this algorithm works surprisingly well. 
# This is primarily because what is usually needed is not a propensity (exact posterior probability) 
# for each record that is accurate in absolute terms but just a reasonably accurate rank 
# ordering of propensities.

# First, we apply a naïve Bayes model with 10-fold cross validation, which gets 83% accuracy. 
# Considering about 83% of our observations in our training set do not attrit, 
# our overall accuracy is no better than if we just predicted “No” attrition for every observation.



# create response and feature data
features <- setdiff(names(train), "Attrition")
x <- train[, features]
y <- train$Attrition

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)

# train model
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

# results
confusionMatrix(nb.m1)

# We can tune the few hyperparameters that a naïve Bayes model has.
#usekernel parameter allows us to use a kernel density estimate for continuous variables 
# versus a guassian density estimate, adjust allows us to adjust the bandwidth of the kernel density 
# (larger numbers mean more flexible density estimate), 
# fL allows us to incorporate the Laplace smoother.

# If we just tuned our model with the above parameters we are able to lift our accuracy to 85%; 
# however, by incorporating some preprocessing of our features 
# (normalize with Box Cox, standardize with center-scaling, and reducing with PCA) 
# we actually get about another 2% lift in our accuracy.


# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 modesl
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))


# plot search grid results
plot(nb.m2)

# results for best model
confusionMatrix(nb.m2)

# We can assess the accuracy on our final holdout test set. 
# Its obvious that our model is not capturing a large percentage of our actual attritions 
# (illustrated by our low specificity).

pred <- predict(nb.m2, newdata = test)
confusionMatrix(pred, test$Attrition)


# start up h2o

h2o.no_progress()
h2o.init()

# create feature names
y <- "Attrition"
x <- setdiff(names(train), y)

# h2o cannot ingest ordered factors
train.h2o <- train %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

# train model
nb.h2o <- h2o.naiveBayes(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 10,
  laplace = 0
)

# assess results
h2o.confusionMatrix(nb.h2o)


# We can also do some feature preprocessing as we did with caret and 
# tune the Laplace smoother using h2o.grid. We don’t see much improvement.

# do a little preprocessing
preprocess <- preProcess(train, method = c("BoxCox", "center", "scale", "pca"))
train_pp   <- predict(preprocess, train)
test_pp    <- predict(preprocess, test)

# convert to h2o objects
train_pp.h2o <- train_pp %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

test_pp.h2o <- test_pp %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

# get new feature names --> PCA preprocessing reduced and changed some features
x <- setdiff(names(train_pp), "Attrition")

# create tuning grid
hyper_params <- list(
  laplace = seq(0, 5, by = 0.5)
)

# build grid search 
grid <- h2o.grid(
  algorithm = "naivebayes",
  grid_id = "nb_grid",
  x = x, 
  y = y, 
  training_frame = train_pp.h2o, 
  nfolds = 10,
  hyper_params = hyper_params
)

# Sort the grid models by mse
sorted_grid <- h2o.getGrid("nb_grid", sort_by = "accuracy", decreasing = TRUE)
sorted_grid

# grab top model id
best_h2o_model <- sorted_grid@model_ids[[1]]
best_model <- h2o.getModel(best_h2o_model)

# confusion matrix of best model
h2o.confusionMatrix(best_model)

# ROC curve
auc <- h2o.auc(best_model, xval = TRUE)
fpr <- h2o.performance(best_model, xval = TRUE) %>% h2o.fpr() %>% .[['fpr']]
tpr <- h2o.performance(best_model, xval = TRUE) %>% h2o.tpr() %>% .[['tpr']]
data.frame(fpr = fpr, tpr = tpr) %>%
  ggplot(aes(fpr, tpr) ) +
  geom_line() + 
  ggtitle( sprintf('AUC: %f', auc) )

# evaluate on test set
h2o.performance(best_model, newdata = test_pp.h2o)

# predict new data
h2o.predict(nb.h2o, newdata = test_pp.h2o)

# shut down h2o
h2o.shutdown(prompt = FALSE)









