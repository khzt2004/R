library(ROCR)
library(car)
library(rms)
library(tidyverse)

# https://github.com/GoogleCloudPlatform/google-analytics-premium-bigquery-statistics
data1 <- read_csv("train_data.csv")

names(data1)

data1 <- data1[c(-1,-2,-12,-22)]

cor(data1)

symnum(abs(cor(data1)),cutpoints = c(0, 0.2, 0.4, 0.6, 0.9, 1), symbols = c(" ", ".", "_", "+", "*"))

data1 <- data1[c(-3,-7)]

data1 <- data1[,c(-13)]

model <- glm(formula = b_CV_flag ~., data = data1, family = binomial("logit"))

result <- summary(model)

result

vif(model)

data1_2 <- data1[,c(-2)]

model1_2 <- glm(formula = b_CV_flag ~., data = data1_2, family = binomial("logit"))

result1_2 <- summary(model1_2)

result1_2

vif(model1_2)

prob <- data.frame(predict(model1_2, data1_2, type = "response"))
gain <- cumsum(sort(prob[, 1], decreasing = TRUE)) / sum(prob)
plot(gain,main ="Gain chart",xlab="number of users", ylab="cumulative conversion rate")

pred <- prediction(prob, data1_2$b_CV_flag)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

qplot(x = perf@x.values[[1]], y = perf@y.values[[1]], xlab = perf@x.name, ylab = perf@y.name, main="ROC curve")

Logistic_Regression_Model <- lrm(b_CV_flag ~., data1_2)
Logistic_Regression_Model

AIC(model)
AIC(model1_2)

coef <- names(model1_2$coefficient)
value <- as.vector(model1_2$coefficient)
result <- data.frame(coef, value)
result

