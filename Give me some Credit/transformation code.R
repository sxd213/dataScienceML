library(ggplot2)

dir <- 'D:/MyStuff/Kaggle/Give me some Credit'
setwd(dir)
train <- read.csv("cs-training.csv",  header = TRUE, sep = ",", row.names = 1)
test <- read.csv("cs-test.csv", header = TRUE, sep = ",", row.names = 1)
train$flg <- "Y"
test$flg <- "N"
data <- rbind(train, test)
names(data) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10','flg')
names(train) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10','flg')
names(test) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10','flg')
dim(data)
# > sum(train$y)/nrow(train)
# [1] 0.06684

# rngdens <- function(df,x,a,b) {
#     mat <- table(ifelse((df$x > a & df$x <= b),'In','out'), df$y)
#     print(mat)
#     print('In:', mat[1,2]/(mat[1,2]+ mat[1,1]) )
#     print('Out:', mat[1,2]/(mat[1,2]+ mat[1,1]) )
# }

#x1:	RevolvingUtilizationOfUnsecuredLines

summary(train$x1)
a <- table(ifelse((train$x1 > 1000 & train$x1 <= 10000),'In','out'), train$y)
print(a)
print (c('In:', a[1,2]/(a[1,2]+ a[1,1])))
print (c('Out:', a[2,2]/(a[2,2]+ a[2,1])))

# > 1 about 10% are bads
#1-2 about 40% are bads
#2-10 about 28% are bads
#10-100 Insignificant
#100-1000 Insignificant
#1000-10000 Insignificant
#10000-100000 Insignificant

summary(train$x1[train$x1 > 2 & train$x1 <= 10])

train$x1bt.1.2 <- ifelse((train$x1 > 1 & train$x1 <= 2),1,0)
table(train$x1bt.1.2)
train$x1bt.2.10 <- ifelse((train$x1 > 2 & train$x1 <= 10),1,0)
train$x1gt.10 <- ifelse((train$x1 > 10),1,0)
train$x1m <- train$x1

#Anything > 
train$x1m[train$x1m > 10 & train$x1m <= 100]<- train$x1m/100
train$x1m[train$x1m > 100 & train$x1m <= 1000]<- train$x1m/1000
train$x1m[train$x1m > 1000 & train$x1m <= 10000]<- train$x1m/10000
train$x1m[train$x1m > 10000 & train$x1m <= 100000]<- train$x1m/100000

summary(train$x1m)

ggplot(train, aes(x = x1m,..density..,fill = as.factor(y)))+   geom_histogram(bins=100, na.rm = TRUE) +
  ggtitle("Rendered Histogram- RevolvingUtilizationOfUnsecuredLines ") +
  xlab("RevolvingUtilizationOfUnsecuredLines (log Scale)") + ylab("Density")+
  guides(fill = guide_legend(title = "SeriousDlqin2yrs"))


# x2: Age

summary(train$x2)
train$x2m <- train$x2
train$x2m[train$x2m > quantile(train$x2m, .95)] <- quantile(train$x2m, .95)
train$x2m[train$x2m < quantile(train$x2m, .05)] <- quantile(train$x2m, .05)
summary(train$x2m)

ggplot(train, aes(x = x2,..density..,fill = as.factor(y))) +   geom_histogram(binwidth = 10, na.rm = TRUE)+
  ggtitle("Rendered Histogram- Age ") +
  xlab("Age") + ylab("Density")+
  guides(fill = guide_legend(title = "SeriousDlqin2yrs"))

# Capped at 95 percentile floored at 5 percentile

#x3: NumberOfTime30-59DaysPastDueNotWorse

summary(train$x3)

a <- table(ifelse((train$x3==0),'In','out'), train$y)
print(a)
print (c('In:', a[1,2]/(a[1,2]+ a[1,1])))
print (c('Out:', a[2,2]/(a[2,2]+ a[2,1])))

length(train$x3[train$x3==0])
train$x3is0 <- ifelse(train$x3==0,1,0)
table(train$x3is0)
train$x3is98 <- ifelse(train$x3==98,1,0)
table(train$x3is98)

a <- table(ifelse((train$x3 > 0 & train$x3 <= 1),'In','out'), train$y)
print(a)
print (c('In:', a[1,2]/(a[1,2]+ a[1,1])))
print (c('Out:', a[2,2]/(a[2,2]+ a[2,1])))
quantile(train$x3, .995)

#x10: NumberOfDependents
summary(train$x10, na.ram = TRUE)
train$x10i <- train$x10
train$x10i[is.na(train$x10)] <- 0
summary(train$x10i, na.ram = TRUE)

length(which(is.na(train$x10)))
a <- table(ifelse((train$x10 <= 20),'In','out'), train$y)
print(a)
print (c('In:', a[1,2]/(a[1,2]+ a[1,1])))
print (c('Out:', a[2,2]/(a[2,2]+ a[2,1])))

ggplot(train, aes(x = x10,..density..,fill = as.factor(y))) +   geom_histogram(bins= 100, na.rm = TRUE)+
  ggtitle("Rendered Histogram- -NumberOfDependents") +
  xlab("NumberOfDependents") + ylab("Frequency")+
  guides(fill = guide_legend(title = "SeriousDlqin2yrs"))

#x5: MonthlyIncome
summary(train$x5)

summary(ifelse(is.na(train$x5)== TRUE,1,train$x5))

idx0 <- which(train$x5==0)
idxna <- which(is.na(train$x5))

x5_tr <- train[-idxna,]

x5_te <- train[idxna,]

summary(x5_te$x10i)

summary(x5_tr$x5)
summary(x5_te$x5)
library(boot)
# #( poly(a,6) +poly(b,6) )^2 x2, x10i
# x5.imp <- glm(formula = x5 ~ ( poly(x2,6) +poly(x10i,6) )^2, family= "gaussian", data = x5_tr)
# summary(x5.imp)
# cv.glm(x5_tr, x5.imp, k=10 )
# x5_te$x10i <- predict(x5.imp,x5_te)
# set.seed(17)
# cv.error.10=rep(0,25)
# n=1
# for (i in 1:5){
#     for (j in 1:5)
#     {
#       
#       x5.imp <- glm(x5 ~ poly(x2,i) +poly(x10i,j)+ x2:x10i, data = x5_tr)
#       cv.error.10[n]=cv.glm(x5_tr,x5.imp,K=10)$delta[1]
#       print(c(i,j,cv.error.10[n]))
#       n = n+1
#     }
# }
# 
# plot(cv.error.10)
# 
# min(cv.error.10)
# which.min(cv.error.10)%%5
# which.min(cv.error.10)%/%5
# cv.error.10 == min(cv.error.10)
set.seed(17)
x5.imp <- glm(x5 ~ x2+x10i+x2:x10i, data = x5_tr)
cv.error=cv.glm(x5_tr,x5.imp,K=10)$delta[1]
print(cv.error)
summary(x5.imp)
x5_tr$x5i <- predict(x5.imp, x5_tr)
summary(x5_tr$x5)
x5_te$x5i <- predict(x5.imp, x5_te)
summary(x5_te$x5i)
print(cv.error)
print(mean((x5_tr$x5i- x5_tr$x5)^2))
summary(x5.imp)

summary(x5_tr[x5_tr$x5 <= quantile(x5_tr$x5 ,.75),])

qplot(
  x5, x5i, data = x5_tr[x5_tr$x5 <= quantile(x5_tr$x5 ,.75),])





#x4: DebtRatio

train$x4debt <- train$x4*ifelse(is.na(train$x5)== TRUE,1,train$x5)
summary(train$x4debt)

summary(train)
