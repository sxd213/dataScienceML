

rm(sales, tsales, sales1, train, test)
dir <- 'D:\\MyStuff\\Housing Price'
setwd(dir)
sales <- read.csv('kc_house_data.csv', stringsAsFactors = FALSE)
#Back up data set
bsales <- sales

sales$saleyrmnth <- as.integer(format(substr(sales$date,1,6), format ="Y%-m%"))
sales$date <-as.Date(paste0(
  format(substr(sales$date,1,4), format = "Y%-m%-d%"),'-',format(substr(sales$date,5,6), format =
                                                                   "Y%-m%-d%"),'-',format(substr(sales$date,7,8), format =
                                                                                            "Y%-m%-d%")
))
unique(sales$saleyrmnth)
# Total Sqft
sales$totsqft <- sales$sqft_living + sales$sqft_lot
# Above sqft to total
sales$sqftabvpct <- sales$sqft_above/(sales$sqft_above+sales$sqft_basement)
# Proportion of bedrooms
sales$bedprop <- sales$bedrooms/(sales$bedrooms+sales$bathrooms+1)
#Age of property
sales$age <- sales$age <-as.numeric(substr(sales$date,1,4))- sales$yr_built
#For building with no renovation imputing year of contruction
sales[sales$yr_renovated==0,"yr_renovated"] <- sales$yr_built[sales$yr_renovated==0]
#Defining new variable Reage
sales$reage <-as.numeric(substr(sales$date,1,4))- sales$yr_renovated
#Ratiof living to lot
sales$liv2lot <- sales$sqft_living/sales$sqft_lot

#Zip Code
aggdata <- aggregate(
  sales$price ~ sales$zipcode, data = sales, FUN = function(x) {
    median(x)
  }
)
names(aggdata) <- c('zipcode','medianprice')
aggdata <- aggdata[order(-aggdata$medianprice),]
head(aggdata,10)
library(gtools)
aggdata$zipdec <-quantcut( aggdata$medianprice, seq(0,1,by=0.1) )
table(aggdata$zipdec)

table(sales$bedrooms)

sales$zipd_98039 <- sales$zipcode- 98039
sales$zipd_98004 <- sales$zipcode- 98004
sales$zipd_98040 <- sales$zipcode- 98040
sales$zipd_98112 <- sales$zipcode- 98112

sales1 <- sales
sales <-
  subset(sales, select = -c(id, date, yr_built, yr_renovated))
lapply(sales, function(x){sum(!is.finite(x))})
# Training and test
set.seed(12345)
smp_size <- 0.8*nrow(sales)
train_ind <- sample(seq_len(nrow(sales)), size = smp_size)

train <- sales[train_ind, ]
test <- sales[train_ind, ]
str(sales)


summary(sales)

#Single variable regression
coln <- names(train)
rsq <- seq(1:ncol(train))
voe<- seq(1:ncol(train))
rsq<-seq(1:ncol(train)) 
intr<- seq(1:ncol(train))
for (i in 1:ncol(train)) {
  Formula <- paste0("train$price~",coln[i])
  print(Formula)
  lm(Formula, train)
    sing.var.reg<-lm(Formula, train)
    rsq[i]<- summary(sing.var.reg)$r.squared
    voe[i]<-sqrt(deviance(sing.var.reg)/df.residual(sing.var.reg))
    intr[i]<- coefficients(sing.var.reg)[1]
  }
sin.var.result <- cbind(coln,rsq,voe,intr)
print(sin.var.result[order(-rsq,-voe),])
# This just shows the 
dimnames(train
         )
#Multiple regression
Mul.var.reg.v2 <- glm(price~ sqft_living+
                        bedrooms*bathrooms+
                        sqft_living+
                        sqft_lot+
                        as.factor(floors)+
                        as.factor(waterfront)+
                        as.factor(view)+
                        as.factor(condition)+
                        as.factor(grade)+
                        sqft_above+
                        sqft_basement+
                        as.factor(zipcode)+
                        poly(lat*long,2)+
                        sqft_living15+
                        sqft_lot15+
                        saleyrmnth+
                        sqftabvpct+
                        bedprop+
                        age+
                        reage+
                        liv2lot, train, family=gaussian())

summary(Mul.var.reg.v2)

predictrain<- predict(Mul.var.reg.v2, train)
sqrt(mse <- mean(residuals(Mul.var.reg.v2)^2))
test$predictest <- predict(Mul.var.reg.v2, test)
sqrt(mean((train$price-predictrain)^2))
sqrt(mean((test$price - predictest)^2))

library(glmnet)
x = model.matrix(price ~ .,sales1)[,-1]
y = sales1$price

library(boot)
cv.error.10=rep(0,10)
Mul.glm.Reg.v1 <- glm(price~ ,train,family=gaussian())
summary(Mul.glm.Reg.v2)
# Grade converted to Grade1 for stability of this algo
for (i in 1:10){
    cv.error.10[i]=cv.glm(train,Mul.var.reg.v2,K=10)$delta[1]
}
#Takes a bit of time for 21K records!
cv.error.10

# Ridge Regression

library(glmnet)
grid = 10 ^ seq(10,-2,length = 100)
Ridge.Mod.v1 = glmnet(x,y,alpha = 0,lambda = grid)
dim(coef(Ridge.Mod.v1))
Ridge.Mod.v1$lambda[50]
coef(Ridge.Mod.v1)[,50]
sqrt(sum(coef(Ridge.Mod.v1)[-1,50] ^ 2))
Ridge.Mod.v1$lambda[60]
coef(Ridge.Mod.v1)[,60]
sqrt(sum(coef(Ridge.Mod.v1)[-1,60] ^ 2))
predict(Ridge.Mod.v1,s = 50,type = "coefficients")[1:20,]
set.seed(12345)
train = sample(1:nrow(x), nrow(x) / 2)
test = (-train)
y.test = y[test]
Ridge.Mod.v2 = glmnet(x[train,],y[train],alpha = 0,lambda = grid, thresh =
                     1e-12)
ridge.pred = predict(Ridge.Mod.v2,s = 4,newx = x[test,])
mean((ridge.pred - y.test) ^ 2)
mean((mean(y[train]) - y.test) ^ 2)
ridge.pred = predict(Ridge.Mod.v2,s = 1e10,newx = x[test,])
mean((ridge.pred - y.test) ^ 2)
ridge.pred = predict(ridge.mod,s = 0,newx = x[test,],exact = T)
mean((ridge.pred - y.test) ^ 2)
lm(y ~ x, subset = train)
predict(Ridge.Mod.v2,s = 0,exact = T,type = "coefficients")[1:20,]
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
ridge.pred = predict(Ridge.Mod.v2,s = bestlam,newx = x[test,])
sqrt(mean((ridge.pred - y.test) ^ 2))
out = glmnet(x,y,alpha = 0)
predict(out,type = "coefficients",s = bestlam)[1:20,]

# The Lasso

lasso.mod.v1 = glmnet(x[train,],y[train],alpha = 1,lambda = grid)
plot(lasso.mod.v1)
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha = 1, k=10)
plot(cv.out)
bestlam = cv.out$lambda.min
print(bestlam)
lasso.pred = predict(lasso.mod.v1,s = bestlam,newx = x[test,])
mean((lasso.pred - y.test) ^ 2)
out = glmnet(x,y,alpha = 1,lambda = grid)
lasso.coef = predict(out,type = "coefficients",s = bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef != 0]

library(caret)
library(mlbench)
#K Nearest Neighbor
knn.mod.v1 <- knnreg(x[train,],y[train], k=5)
# summarize the fit
print(knn.mod.v1)
# make predictions
predictions <- predict(knn.mod.v1, x[train,])
# summarize accuracy
mse <- mean((y[train] - predictions)^2)
print(sqrt(mse))

#SVM Radial basis model not built due to execution time issues


# # load libraries
# library(caret)
# library(mlbench)
# 
# control <- trainControl(method="cv", number=5)
# svmRadial.mod.v1 <- train(price~., data=train, method="svmRadial", metric="RMSE", trControl=control)
# # summarize fit
# print(svmRadial.mod.v1)

library(rpart)
# fit model
rpart.mod.v1 <- rpart(price~., data=train, control=rpart.control(minsplit=5))
# summarize the fit
print(rpart.mod.v1)
# make predictions
predictions <- predict(rpart.mod.v1, train[,-1])
# summarize accuracy
mse <- mean((train$price - predictions)^2)
print(sqrt(mse))

# Ensemble model






