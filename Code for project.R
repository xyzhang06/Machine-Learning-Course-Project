# Set working directory
setwd("C:/Users/Mansen/Documents/Xiuyun/Coursera/Course 8 - Pratical Machine Learning/Course Project")

# Loading data
url1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

download.file(url1, destfile ="./training.csv", method = "curl")
download.file(url2, destfile ="./testing.csv", method = "curl")

training <- read.csv("./training.csv", header = TRUE)
testing <- read.csv("./testing.csv", header = TRUE)

# Clean data: deleting columns that are variables on summary statistics
training.new <- training[, -which(grepl('user_name|X|timestamp|window|kurtosis_|skewness_|max_|min_|
                                        amplitude_|avg_|var_|stddev_|amplitude', names(training)))]

testing.new <- testing[, -which(grepl('user_name|X|timestamp|window|kurtosis_|skewness_|max_|min_|
                                      amplitude_|avg_|var_|stddev_|amplitude', names(testing)))]

# Split training data into train and test datasets
library(caret)
library(gbm)
library(AppliedPredictiveModeling)
library(randomForest)

set.seed(1001)
intrain <- createDataPartition(training.new$classe, p = 0.4, list = FALSE)
train.sub <- training.new[intrain,]
test.sub<- training.new[-intrain,]

# Model fitting
modfit.rpart <- train(classe ~., data = train.sub, method = "rpart")
modfit.rf <- randomForest(classe ~., data = train.sub)

pred.rpart <- predict(modfit.rpart, test.sub)
confusionMatrix(pred.rpart, test.sub$classe)

pred.rf <- predict(modfit.rf, test.sub)
confusionMatrix(pred.rf, test.sub$classe)

# Predit for final test data
pred.test <- predict(modfit.rf, testing.new)
print(pred.test)