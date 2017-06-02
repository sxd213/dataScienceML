setwd('D:\\MyStuff\\Cousera\\ML Regression- UW\\wk3_kc_house_test_data.csv')
test <- read.csv('wk3_kc_house_test_data.csv', stringsAsFactors = FALSE)

setwd('D:\\MyStuff\\Cousera\\ML Regression- UW\\wk3_kc_house_train_data.csv')
train <- read.csv('wk3_kc_house_train_data.csv', stringsAsFactors = FALSE)

setwd('D:\\MyStuff\\Cousera\\ML Regression- UW\\wk3_kc_house_valid_data.csv')
valid <- read.csv('wk3_kc_house_valid_data.csv', stringsAsFactors = FALSE)




names(df.dates) <- c("yrmnth","file")

df.dates[.1] <- as.integer(df.dates[,1])

class(df.dates[,1])

df.dates <-as.data.frame(df.dates)
class(df.dates)

table(df.dates[,1], df.dates[,2])

aggregate(
  yrmnth~ file, data = df.dates, FUN = function(x) {
    length(x)
  }
)

#=======================================================
#=======================================================

1
2
3
4
5
# load the library
library(mlbench)
# load data
data(BostonHousing)
# fit model
fit <- lm(medv~., BostonHousing)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, BostonHousing)
# summarize accuracy
mse <- mean(residuals(fit)^2)
print(mse)
# Root mean squared error
rmse <- sqrt(mse)
# Residual sum of squares
rss <- sum(residuals(fit)^2)
# Residual standard error
rse <- sqrt( sum(residuals(fit)^2) / fit$df.residual ) 

# load the libraries
library(glmnet)
library(mlbench)

BostonHousing$chas <- as.numeric(as.character(BostonHousing$chas))
x <- as.matrix(BostonHousing[,1:13])
y <- as.matrix(BostonHousing[,14])
# fit model
fit <- glmnet(x, y, family="gaussian", alpha=0.5, lambda=0.001)
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, x, type="link")
# summarize accuracy
mse <- mean((y - predictions)^2)
print(mse)

mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)

mse <- mean(residuals(fit)^2)
print(mse)
# Root mean squared error
rmse <- sqrt(mse)
# Residual sum of squares
rss <- sum(residuals(fit)^2)
# Residual standard error
rse <- sqrt( sum(residuals(fit)^2) / fit$df.residual )

#=================
#=KNN Implementation

library(caret)
library(mlbench)

knn.model.v1 <- knnreg(x, y, k=15)
# summarize the fit
print(knn.model.v1)
# make predictions
predictions <- predict(fit, x)
# summarize accuracy
mse <- mean((train$ - predictions)^2)
print(mse)

mse <- mean(residuals(fit)^2)
print(mse)
# Root mean squared error
rmse <- sqrt(mse)
# Residual sum of squares
rss <- sum(residuals(fit)^2)
# Residual standard error
rse <- sqrt( sum(residuals(fit)^2) / fit$df.residual )

#==============
set.seed(7)
control <- trainControl(method="cv", number=5)
fit.knn <- train(medv~., data=BostonHousing, method="knn", metric="RMSE", preProc=c("center", "scale"), trControl=control)
# summarize fit
print(fit.knn)
print(mse)
# Root mean squared error
rmse <- sqrt(mse)
# Residual sum of squares
rss <- sum(residuals(fit)^2)
# Residual standard error
rse <- sqrt( sum(residuals(fit)^2) / fit$df.residual )
#================================
SVM
#=====================

fit <- ksvm(medv~., BostonHousing, kernel="rbfdot")
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, BostonHousing)
# summarize accuracy
mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)

# Mean squared error
mse <- mean(residuals(fit)^2)
# Root mean squared error
rmse <- sqrt(mse)
# Residual sum of squares
rss <- sum(residuals(fit)^2)
# Residual standard error
rse <- sqrt( sum(residuals(fit)^2) / fit$df.residual )
#===============================
#Rpart implementation
#===============================

fit <- rpart(medv~., data=BostonHousing, control=rpart.control(minsplit=5))
# summarize the fit
print(fit)
# make predictions
predictions <- predict(fit, BostonHousing[,1:13])
# summarize accuracy
mse <- mean((BostonHousing$medv - predictions)^2)
print(mse)

# Mean squared error
mse <- mean(residuals(fit)^2)
# Root mean squared error
rmse <- sqrt(mse)
# Residual sum of squares
rss <- sum(residuals(fit)^2)
# Residual standard error
rse <- sqrt( sum(residuals(fit)^2) / fit$df.residual )

# #===================
# set.seed(17)
# cv.error.10=rep(0,10)
# for (i in 1:10){
#   fit <- lm(medv~., BostonHousing)
#   cv.error.10[i]=cv.glm(BostonHousing, fit,K=10)$delta[1]
# }
# cv.error.10

my_features_model <- lm(formula=price ~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + zipcode, data=train_data)



PredictorVariables <- paste(names(train), sep="")
Formula <- formula(paste("train$price ~ ", 
                         paste(names(train[,1]), collapse=" + ")))
lm(Formula, train)

coln <- names(train)
rsq <- seq(1:ncol(train))
resi<- seq(1:ncol(train))

for (i in 1:ncol(train)) {
  Formula <- paste0("train$price~",coln[i])
  sing.var.reg<-lm(Formula, train)
  rsq[i]<- summary(sing.var.reg)$r.squared
  resi[i]<-summary(sing.var.reg)$residuals
}




