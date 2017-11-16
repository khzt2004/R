# http://r-statistics.co/Logistic-Regression-With-R.html

library(smbinning)
library(InformationValue)
library(car)

inputData <- read.csv("adult.csv")
# inputdata <- read.csv("HVA_event_sample")

head(inputData)

# Check Class bias
table(inputData$ABOVE50K)

# Create Training Data
input_ones <- inputData[which(inputData$ABOVE50K == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$ABOVE50K == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

# The smbinning::smbinning function converts a continuous variable 
#into a categorical variable using recursive partitioning. 
#We will first convert them to categorical variables 
# and then, capture the information values for all variables in iv_df

# segregate continuous and factor variables
factor_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")
continuous_vars <- c("AGE", "FNLWGT","EDUCATIONNUM", "HOURSPERWEEK", "CAPITALGAIN", "CAPITALLOSS")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(14))  # init for IV results

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="ABOVE50K", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="ABOVE50K", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df

# Build Logit Models and Predict

logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores

optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] 
summary(logitMod)

# VIF
# Like in case of linear regression, we should check for multicollinearity
# in the model. As seen below, all X variables in the model have VIF well below 4.

vif(logitMod)
misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)
plotROC(testData$ABOVE50K, predicted)

Concordance(testData$ABOVE50K, predicted)

sensitivity(testData$ABOVE50K, predicted, threshold = optCutOff)

specificity(testData$ABOVE50K, predicted, threshold = optCutOff)

confusionMatrix(testData$ABOVE50K, predicted, threshold = optCutOff)

