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



summary(train$x1[(train$x1 >10 & train$x1 <=1000000)])
a <- table(ifelse((train$x1 >2 & train$x1 <=10),'In','out'), train$y)
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

train$x1bt.1.2 <- as.factor(ifelse((train$x1 > 1 & train$x1 <= 2),1,0))
table(train$x1bt.1.2)
train$x1bt.2.10 <- as.factor(ifelse((train$x1 > 2 & train$x1 <= 10),1,0))
train$x1gt.10 <- as.factor(ifelse((train$x1 > 10),1,0))
train$x1bt.0.1 <- as.factor(ifelse((train$x1 > 0 & train$x1 <= 1),1,0))

#Anything > 
train$x1m <- train$x1
train$x1m <- ifelse((train$x1m > 2 & train$x1m <= 10),train$x1m/10, train$x1m)
train$x1m <-ifelse((train$x1m > 10 & train$x1m <= 100),train$x1m/100, train$x1m)
train$x1m <-ifelse((train$x1m > 100 & train$x1m <= 1000),train$x1m/1000, train$x1m)
train$x1m <-ifelse((train$x1m > 1000 & train$x1m <= 10000),train$x1m/10000, train$x1m)
train$x1m <-ifelse((train$x1m > 10000 & train$x1m <= 100000),train$x1m/100000, train$x1m)
train$x1m <-ifelse((train$x1m > 100000 & train$x1m <= 1000000),train$x1m/1000000, train$x1m)


summary(train$x1m)
which(!is.finite(train$x1m))



ggplot(train, aes(x = log(x1m),..density..,fill = as.factor(y)))+   geom_histogram(bins=50, na.rm = TRUE) +
  ggtitle("Rendered Histogram- RevolvingUtilizationOfUnsecuredLines ") +
  xlab("RevolvingUtilizationOfUnsecuredLines (log Scale)") + ylab("Density")+
  guides(fill = guide_legend(title = "SeriousDlqin2yrs"))

train$x1[train$x1m < .01]
#========================================

# x2: Age



summary(train$x2)
train$x2m <- train$x2

train$x2m[train$x2m > quantile(train$x2m, .95)] <- quantile(train$x2m, .95)
train$x2m[train$x2m < quantile(train$x2m, .05)] <- quantile(train$x2m, .05)
train$x2o <-
  ifelse(train$x2 > quantile(train$x2, .95) &
           train$x2 < quantile(train$x2, .05),1,0)
summary(train$x2m)

ggplot(train, aes(x = x2,..density..,fill = as.factor(y))) +   geom_histogram(binwidth = 10, na.rm = TRUE)+
  ggtitle("Rendered Histogram- Age ") +
  xlab("Age") + ylab("Density")+
  guides(fill = guide_legend(title = "SeriousDlqin2yrs"))

# Capped at 95 percentile floored at 5 percentile

#x3: NumberOfTime30-59DaysPastDueNotWorse
#x7:	NumberOfTimes90DaysLate
#x9:	NumberOfTime60-89DaysPastDueNotWorse

length(which(is.finite(train$x9)))

length(train$x3[train$x7 == 96 & train$x9 == 96])

summary(train$x3)

a <- table(ifelse((train$x3 > train$x7+train$x9),'In','out'), train$y)
print(a)
print (c('In:', a[1,2]/(a[1,2]+ a[1,1])))
print (c('Out:', a[2,2]/(a[2,2]+ a[2,1])))

train$x397h <- as.factor(ifelse((train$x7 > train$x3+train$x9),1,0))
train$x397m <- as.factor(ifelse((train$x9 > train$x3+train$x7),1,0))
train$x397l <- as.factor(ifelse((train$x3 > train$x7+train$x9),1,0))

length(train$x3[train$x3==0])
train$x3is0 <- as.factor(ifelse(train$x3==0,1,0))
table(train$x3is0)
train$x3gt90 <- as.factor(ifelse(train$x3 > 90,1,0))
table(train$x3gt90)
train$x3gt95pct <- as.factor(ifelse(train$x3 > quantile(train$x3,0.95),1,0))

train$x9is0 <- as.factor(ifelse(train$x9==0,1,0))
table(train$x9is0)
train$x9gt90 <- as.factor(ifelse(train$x9 > 98,1,0))
table(train$x3gt90)
train$x9gt95pct <- as.factor(ifelse(train$x9 > quantile(train$x9,0.95),1,0))

train$x7is0 <- as.factor(ifelse(train$x7==0,1,0))
table(train$x7is0)
train$x7gt90 <- as.factor(ifelse(train$x7 > 90,1,0))
table(train$x7gt90)
train$x7gt75pct <- as.factor(ifelse(train$x7 > quantile(train$x7gt75pct[train$x7gt75pct < 90],0.75),1,0))

table(train$x9[train$x3 > 90 | train$x7 > 90 | train$x9 > 90])

train$x397alldpd <-
  ifelse(train$x3 < 90 |
           train$x7 < 90 |
           train$x9 < 90, train$x3 + train$x7 + train$x9, train$x3)
summary(train$x397alldpd[ train$x397alldpd< 90])


train$x397alldpdis0 <- as.factor(ifelse(train$x397alldpd==0,1,0))
table(train$x397alldpdis0)
train$x397alldpdgt90 <- as.factor(ifelse(train$x397alldpd > 90,1,0))
table(train$x397alldpdgt90)
train$x397alldpdgt95pct <-
  as.factor(ifelse(train$x397alldpd > quantile(train$x397alldpd[train$x397alldpd < 90],0.95),1,0))
train$x397alldpdgt75pct <-
  as.factor(ifelse(train$x397alldpd > quantile(train$x397alldpd[train$x397alldpd < 90],0.75),1,0))


a <- table(ifelse((train$x3 > 0 & train$x3 <= 1),'In','out'), train$y)
print(a)
print (c('In:', a[1,2]/(a[1,2]+ a[1,1])))
print (c('Out:', a[2,2]/(a[2,2]+ a[2,1])))
quantile(train$x3, .95)



#x10: NumberOfDependents
summary(train$x10, na.ram = TRUE)
train$x10isNA <- as.factor(ifelse(is.na(train$x10),1,0))
train$x10is0 <- as.factor(ifelse(train$x10==0,1,0))
train$x10gt95pct <- as.factor(ifelse(train$x10 >quantile(train$x10,.95, na.rm = TRUE),1,0))
train$x10i <- train$x10
train$x10gt75pct <- as.factor(ifelse(train$x10 >quantile(train$x10,.75, na.rm = TRUE),1,0))
# MEdian value imputation
train$x10i[is.na(train$x10)] <- 0
summary(train$x10i, na.ram = TRUE)

length(which(is.na(train$x10)))
a <- table(ifelse((train$x10 <= 20),'In','out'), train$y)
print(a)
print (c('In:', a[1,2]/(a[1,2]+ a[1,1])))
print (c('Out:', a[2,2]/(a[2,2]+ a[2,1])))

ggplot(train, aes(x = x10i,..density..,fill = as.factor(y))) +   geom_histogram(bins= 100, na.rm = TRUE)+
  ggtitle("Rendered Histogram- -NumberOfDependents") +
  xlab("NumberOfDependents") + ylab("Frequency")+
  guides(fill = guide_legend(title = "SeriousDlqin2yrs"))

#x5: MonthlyIncome
summary(train$x5i)
train$x5isNULL <- as.factor(ifelse(is.na(train$x5),1,0))
table(train$x10gt95pct)
train$x5is0 <- as.factor(ifelse(train$x5==0,1,0))
train$x5i <- train$x5
train$x5i[is.na(train$x5i)] <- median(train$x5, na.rm = TRUE)
summary(train$x5i)
summary(train$x5)


#x4: DebtRatio
summary(train$x4)
train$x4debt <- train$x4*ifelse(is.na(train$x5)== TRUE | train$x5==0,1,train$x5)
train$x4log <- log1p(train$x4debt)
summary(train$x4log)


summary(train$x4debt)
# train$x4i <- train$x4debt/ train$x5i
# summary(train$x4i)
train$x4incm.dbt.gap <- train$x5i- train$x4debt
summary(train$x4incm.dbt.gap)

ggplot(train, aes(x = scale(train$x4incm.dbt.gap),..density..,fill = as.factor(y))) +   geom_histogram(bins  = 250 , na.rm = TRUE)+
  ggtitle("Rendered Histogram- NumberOfTime30-59DaysPastDueNotWorse ") +
  xlab("Income Debt Gap") + ylab("Density")+
  guides(fill = guide_legend(title = "SeriousDlqin2yrs"))

#x6: NumberOfOpenCreditLinesAndLoans
summary(train$x6- train$x8)
length(which(train$x6 < train$x8))
train$x6cc <- train$x6- train$x8

train$x8ge6cc <- as.factor(ifelse(train$x6cc >= train$x8,1,0))
table(train$x8ge6cc, train$y)

table(ifelse(train$x6lc < train$x8,1,-1))
summary(train$x6)
summary(train$x6lc)
#x8: NumberRealEstateLoansOrLines
summary(train$x8)
train$x6[train$x8==54]
plot(train$x6lc, train$x8)

train$x6co <- as.factor(ifelse(train$x4debt > 0 & train$x6==0,1,0))
train$x4avdbt <- train$x4debt/(train$x6+1)
train$x4lavdbt <- log1p(train$x4avdbt)

ggplot(train, aes(x = x4lavdbt,..density..,fill = as.factor(y))) +   geom_histogram(bins  = 10 , na.rm = TRUE)+
  ggtitle("Rendered Histogram- NumberOfTime30-59DaysPastDueNotWorse ") +
  xlab("Avg Debt/ Instrument") + ylab("Density")+
  guides(fill = guide_legend(title = "SeriousDlqin2yrs"))
