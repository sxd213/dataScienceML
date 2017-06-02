

rm(sales, tsales, sales1, train, test)
dir <- 'D:\\MyStuff\\Housing Price'
setwd(dir)
sales <- read.csv('kc_house_data.csv', stringsAsFactors = FALSE)
#Back up data set
tsales <- read.csv('kc_house_data.csv', stringsAsFactors = FALSE)

summary(sales)

# Exploring Basic structure
cl <- colnames(sales)
length(sales)
str(sales)

# Exploring the Data 
library(gtools)

# Creating a price decile across entire data set for rendering
sales$pricdec <-quantcut( sales$price, seq(0,1,by=0.1) )
aggregate(
  price ~ pricdec, data = sales, FUN = function(x) {
    length(x)
  }
)

#date" 
#Extracting Year and Month of sale
sales$saleyrmnth <- format(substr(sales$date,1,6), format ="Y%-m%")
unique(sales$saleyrmnth)
# Extracting Month
sales$mnth <- format(substr(sales$date,5,6), format ="m%")
sales$mnth <- as.integer(sales$mnth)

# Exploring means of each month
aggregate(
  price ~ saleyrmnth, data = sales, FUN = function(x) {
    mean(x)
  }
)

#Converting to factor in order to make it easy to use in exploring
sales$saleyrmnth<- as.factor(sales$saleyrmnth)

#Graphics library 
library(ggplot2)

ggplot(sales, aes(sales$saleyrmnth, fill=sales$pricdec)) + geom_bar()+
  xlab("Month")
# Definite peaks and troughs in data. Compisition of high end and low ends
# don't seem to vary across month
#Boxplot
ggplot(sales, aes(as.factor(saleyrmnth),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Sqft lot in Seattle") +
  xlab("Month") + ylab("Price")

# Boxplot for yrmnth
ggplot(sales, aes(as.factor(mnth),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Sqft lot in Seattle") +
  xlab("Month") + ylab("Price")

## Add sunshine hours as information and rank order months by sunshine
# There is a trend that the winter and rainy months have throughs in sales, 
#summer and spring sees upswings. Hence in order to get additional information mapped
#hours sunlight to the months. Using Seattle as a surrogate for the whole
#http://www.usclimatedata.com/climate/seattle/washington/united-states/uswa0395
x <-sort(unique(sales$mnth))
y <- c(74,99,154,201,247,234,304,248,197,122,77,62)
df <- data.frame(x,y)
names(df) <- c("month","sunshine")
df$ssr <- rank(df$sunshine)
for (i in 1:nrow(df)){
  sales$ssr[sales$mnth == df[i,1]] <- df[i,3]
}
qplot(
  as.factor(sales$ssr), data = sales, geom = "bar", fill = sales$pricdec, position =
    "fill"
)+ xlab("Sunshine Rank")+ ylab("# Transactrion")
sales$ssr <- as.factor(sales$ssr)

#Let's get ambitious. Does susnshine impact price?
ggplot(sales, aes(as.factor(ssr),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Sqft lot in Seattle") +
  xlab("Month rank in sunshine") + ylab("Price")
#Nah!! not that much. But will keep the ssr around because of the upward trend

#"bedrooms" 
aggregate(
  price ~ bedrooms, data = sales, FUN = function(x) {
    mean(x)
  }
)
# There is some trend in the pricing. Will keep 0 bedrooms as of now as 
#long as there is a sqft_living

head(sales[sales$bedrooms == 2,])

# Mild trend observed
ggplot(sales, aes(as.factor(bedrooms^2),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Bedrooms") + ylab("Price")

#What happens if the bedroom is squared?
#The differentiation is now lot clearer
sales$sqbedrooms<- as.numeric(sales$bedrooms)^2
sales$bedrooms<- as.numeric(sales$bedrooms)

#[5] "bathrooms"   

ggplot(sales, aes(as.factor(bathrooms),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Bathrooms") + ylab("Price")
#Definite trend by bathroom!

sales$bathrooms <- as.numeric(sales$bathrooms)
unique(sales$bathrooms)

summary(sales$bathrooms)
#Lets check out bedroom&bathroom interaction
sales$bb <- as.numeric(sales$bathrooms)*as.numeric(sales$bedrooms)

summary(sales$bb)

ggplot(sales, aes(as.factor(bb),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Bath/Bed") + ylab("Price")
#There is a general upward trend. Although dispersed in the higher
#echelons of data
qplot(sales$bb,sales$price,colour=sales$price,
      xlab ="Square feet", ylab="price($)")+
  geom_smooth(method = "lm")

#Definite upward trend

#"sqft_living"
qplot(sales$sqft_living,sales$price,colour= sales$price,
      xlab ="Square feet", ylab="price($)")+
  geom_smooth(method = "lm")
#Definite upward trend
sales$sqft_living <- as.numeric(sales$sqft_living)

#"sqft_lot" 
qplot(sales$sqft_lot,sales$price,colour= sales$price,
      xlab ="Square feet", ylab="price($)")+
  geom_smooth(method = "lm")
# Yes the linearity is expected

sales$sqft_lot <- as.numeric(sales$sqft_lot)

sales$lotbyliving<- sales$sqft_lot/sales$sqft_living
summary(sales$lotbyliving)

qplot(1/sales$lotbyliving,sales$price,colour= sales$price,
      xlab ="Lot by Living ", ylab="price($)")+
  geom_smooth(method = "lm")
#Looks interesting

##"floors"
ggplot(sales, aes(as.factor(floors),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Floors") + ylab("Price")

#There is some change based on the number of floors

sales$floors <- as.factor(sales$floors)

#[9] "waterfront"   
ggplot(sales, aes(as.factor(waterfront),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Waterfront") + ylab("Price")

#Wow!! water front properties have higher value

sales$waterfront <- as.factor(sales$waterfront)

lm(sales$price~sales$waterfront)
#Just checked for R accepting this variable as a input for linear model


#"view"          
ggplot(sales, aes(as.factor(view),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("View") + ylab("Price")

#View definitely have a impact

sales$view <- as.factor(sales$view)

#"condition"     
ggplot(sales, aes(as.factor(condition),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Condition") + ylab("Price")

sales$condition <- as.factor(sales$condition)
#Condition has a sligh positive impact visually

#"grade" 
class(sales$grade)

table(sales$grade1)
sales$grade1 <- as.character(sales$grade)
sales$grade1[sales$grade > 10] <- '>10'
sales$grade1[sales$grade < 6] <- '<6'

sales$grade1 <- as.factor(sales$grade1)
summary(sales$grade1)

ggplot(sales, aes(as.factor(grade),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Building Grade") +
  xlab("Grade")+ ylab("Price")

qplot(sales$grade,sales$price,colour= sales$price,
      xlab ="Grade", ylab="price($)")+
  geom_smooth(method = "lm")

sales$grade <- as.factor(sales$grade)
#Improvement in Grade definitely has a impact on improving 


#[13] "sqft_above"    "sqft_basement" 

nrow(sales[sales$sqft_living == sales$sqft_above+sales$sqft_basement,])
#Indeed!! Sqft living add up to above and basement

qplot(sales$sqft_living ,sales$price,colour= sales$price,
      xlab ="Square feet", ylab="price($)")+
  geom_smooth(method = "lm", se = TRUE)

#Some quick transformation      
sales$sqft_above <- as.numeric(sales$sqft_above)
sales$sqft_basement <- as.numeric(sales$sqft_basement)
sales$basement2all<- as.numeric(sales$sqft_basement)/(as.numeric(sales$sqft_above)+as.numeric(sales$sqft_basement))
sales$above2all<- as.numeric(sales$sqft_above)/(as.numeric(sales$sqft_above)+as.numeric(sales$sqft_basement))
summary(sales$basement2all)
# 'To all ratios' prevent div by 0- NaNs/Na

#"yr_built" 

class(sales$yr_built)
sales$age <-as.numeric(substr(sales$date,1,4))- sales$yr_built
#Tenurizing everything
head(sales[sales$age == 109,])
# Actually that old a building!!

qplot(sales$age ,sales$price,colour= sales$price,
      xlab ="Age", ylab="price($)")+
  geom_smooth(method = "lm", se = TRUE)
#No obvious visual corelation

#"yr_renovated"

#For building with no renovation imputing year of contruction
sales[sales$yr_renovated==0,"yr_renovated"] <- sales$yr_built[sales$yr_renovated==0]
#Defining new variable Reage
sales$reage <-as.integer(substr(sales$date,1,4))- sales$yr_renovated
qplot(sales$reage ,sales$price,colour= sales$price,
      xlab ="Age", ylab="price($)")

sales$reage <- as.numeric(sales$reage)

#[17] "zipcode"  

str(sales$zipcode)

aggregate(
  price ~ zipcode, data = sales, FUN = function(x) {
    length(x)
  }
)

ggplot(sales, aes(as.factor(zipcode),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Zipcode in Seattle") +
  xlab("Zipcode")+ ylab("Price")
#Significant change in prices by Zipcode
#Very discrete in appearance

sales$zipcode <- as.factor(sales$zipcode)

qplot(sales$zipcode ,sales$price,colour= sales$pricdec,
      xlab ="Zipcode", ylab="price($)")

#"sqft_living15"
qplot(sales$sqft_living ,sales$sqft_living15,colour= sales$price,
      xlab ="Square feet", ylab="sqft living15")+
  geom_smooth(method = "lm", se = TRUE)
#High correlation evident
#[21] "sqft_lot15"
qplot(sales$sqft_lot ,sales$sqft_lot15,colour= sales$price,
      xlab ="Sqft lot", ylab="sqft lot15")+
  geom_smooth(method = "lm", se = TRUE)
# Positive Correlation

sales$sqft_living15 <- as.numeric(sales$sqft_living15)
sales$sqft_lot15 <- as.numeric(sales$sqft_lot15)
sales$lotbyliving15<- as.numeric(sales$sqft_lot15)/ as.numeric(sales$sqft_living15)

qplot(sales$lotbyliving15 ,sales$sqft_lot15,colour= sales$price,
      xlab ="LotbyLiving", ylab="sqft lot15")+
  geom_smooth(method = "lm", se = TRUE)
#Positive Correlation

# price
library(ggplot2)
qplot(sales$sqft_living,sales$price,colour=sales$price,
      xlab ="Squre feet", ylab="price($)")

sales$dpsqft <- sales$price/ (sales$sqft_living+sales$sqft_lot)

ggplot(sales, aes(as.factor(zipcode),dpsqft)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Zipcode in Seattle") +
  xlab("Zipcode")+ ylab("psqft price")

ggplot(sales, aes(as.factor(bb),dpsqft)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Zipcode in Seattle") +
  xlab("BB interaction")+ ylab("psqft price")

aggregate(
  dpsqft ~ bb+zipcode, data = sales, FUN = function(x) {
    mean(x)
  }
)

sales$dpsqft15 <- sales$price/ (sales$sqft_living15+sales$sqft_lot15)

ggplot(sales, aes(as.factor(zipcode),dpsqft15)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Zipcode in Seattle") +
  xlab("Zipcode")+ ylab("psqft price")

ggplot(sales, aes(as.factor(bb),dpsqft15)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Zipcode in Seattle") +
  xlab("BB interaction")+ ylab("psqft price")

#$ per sqft appears to be a interesting target metric to predict
# But a few reasons to avoid:
# 1. Errors will get multiplied by sqft to estimate final price
# For large sqft age th errors might just ger multiplied
# 2. All independent vectors have to be appropriately treated as the target
# variable is no longher $ but $/sqft. Coefficients otherwise may have
#different dimensionality

sales$saleyrmnth <- as.integer(sales$saleyrmnth)


colnames(sales)
str(sales)

summary(as.boolean(sales$waterfront))
#Preparing data for initial model
# Taking out some variables that were created for initial exploration

# Dsitance from the Space Needle- Center of the town. The hypothesis
# here is that anything closer to city ought to be more expensive
#Haversine distance is a means to calculate geodesic distane along the 
#curvature of the earth
haversine<- function(long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  long1<- -122.3493
  lat1 <- 47.6205
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

sales$spnedist <- mapply(haversine, sales$long, sales$lat)
summary(sales$spnedist)
sales$lspnedist <- log(sales$spnedist)

sales$price <- tsales$price
qplot(sales$lspnedist ,sales$price,colour= sales$price,
      xlab ="Distance from Space Needle", ylab="price($)")+
  geom_smooth(method = "lm", se = TRUE)


# Taking off variables that have been used to calculate new variables
# Also, some that would introduce obvious multicollinearity
str(sales)
sales1 <-
  subset(
    sales, select = -c(
      id, date, dpsqft, bedrooms, sqft_lot, spnedist, ssr, bathrooms, grade, basement2all, above2all, sqft_basement, sqft_lot15, dpsqft15,pricdec,lat,long, mnth, yr_built, yr_renovated
    )
  )

## set the seed to make your partition reproductible
set.seed(12345)
smp_size <- 0.8*nrow(sales1)
train_ind <- sample(seq_len(nrow(sales1)), size = smp_size)

train <- sales1[train_ind, ]
test <- sales1[-train_ind, ]

lapply(sales1[train_ind, ], function(x){sum(!is.finite(x))})
lapply(sales1[-train_ind, ], function(x){sum(!is.finite(x))})

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

#Multiple regression
Mul.var.reg.v2 <- lm(train$price~., train)

summary(Mul.var.reg.v2)

predictrain<- predict(Mul.var.reg.v2, train)
sqrt(mse <- mean(residuals(Mul.var.reg.v2)^2))
predictest <- predict(Mul.var.reg.v2, test)
sqrt(mean((train$price-predictrain)^2))
sqrt(mean((test$price - predictest)^2))

library(glmnet)
x = model.matrix(price ~ .,sales1)[,-1]
y = sales1$price

library(boot)
cv.error.10=rep(0,10)
Mul.glm.Reg.v1 <- glm(price~.,train,family=gaussian())
summary(Mul.glm.Reg.v1)
# Grade converted to Grade1 for stability of this algo
for (i in 1:10){
    cv.error.10[i]=cv.glm(train,Mul.glm.Reg.v1,K=10)$delta[1]
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






