library(mice)
library(randomForest)
library(caret)


raw_data <- read.csv('C:/Users/Koteki/Downloads/SAPio_DataScience_Challenge.csv')

# view a summary of the dataset
summary(raw_data)

# numeric variables
nums <- sapply(raw_data, is.numeric)
num_data <- raw_data[,nums]

# inspect the relationship between residual.sugar and response
cor(num_data, method='pearson',use='complete.obs')

# drop residual.sugar as 1/3 are missing, and weak correlation
raw_data$residual.sugar <- NULL

# convert quality to factor
raw_data$quality <- as.factor(raw_data$quality)

# impute missing values and check distribution
imputed_data <- complete(mice(raw_data))
summary(imputed_data)

# split dataset into training and validation (80/20 split)
set.seed(35)
n <- nrow(imputed_data)
gp <- runif(n)
train <- imputed_data[gp<0.8,]
valid <- imputed_data[gp>=0.8, ]

# random forest
rf_fit <- randomForest(quality ~ ., data=train,ntree=1000)
varImpPlot(rf_fit)

# predict test data
rf_test <- predict(rf_fit,valid,type='class')

# confusion matrix
conf <- table(valid$quality,rf_test)

# calculate accuracy (68%)
acc <- sum(diag(conf)/sum(conf))
 



