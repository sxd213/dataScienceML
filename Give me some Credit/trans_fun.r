

{
 # x1 transformation
train$x1bt.2.10 <- ifelse((train$x1 > 2 & train$x1 <= 10),1,0)
train$x1gt.10 <- ifelse((train$x1 > 10),1,0)
train$x1bt.1.2 <- ifelse((train$x1 > 1 & train$x1 <= 2),1,0)
train$x1bt.0.1 <- ifelse((train$x1 > 0 & train$x1 <= 1),1,0)
train$x1m <- train$x1
train$x1m <- ifelse((train$x1m > 2 & train$x1m <= 10),train$x1m/10, train$x1m)
train$x1m <-ifelse((train$x1m > 10 & train$x1m <= 100),train$x1m/100, train$x1m)
train$x1m <-ifelse((train$x1m > 100 & train$x1m <= 1000),train$x1m/1000, train$x1m)
train$x1m <-ifelse((train$x1m > 1000 & train$x1m <= 10000),train$x1m/10000, train$x1m)
train$x1m <-ifelse((train$x1m > 10000 & train$x1m <= 100000),train$x1m/100000, train$x1m)
train$x1m <-ifelse((train$x1m > 100000 & train$x1m <= 1000000),train$x1m/1000000, train$x1m)
train$x1log <- log(train$x1)

#x2 transformation

train$x2m <- train$x2

train$x2m[train$x2m > quantile(train$x2m, .95)] <- quantile(train$x2m, .95)
train$x2m[train$x2m < quantile(train$x2m, .05)] <- quantile(train$x2m, .05)
train$x2o <-
  ifelse(train$x2 > quantile(train$x2, .95) &
           train$x2 < quantile(train$x2, .05),1,0)

#x3: NumberOfTime30-59DaysPastDueNotWorse
#x7:	NumberOfTimes90DaysLate
#x9:	NumberOfTime60-89DaysPastDueNotWorse
train$x3is0 <- ifelse(train$x3==0,1,0)
train$x3gt90 <- ifelse(train$x3 > 90,1,0)
train$x3gt95pct <- ifelse(train$x3 > quantile(train$x3,0.95),1,0)
train$x9is0 <- ifelse(train$x9==0,1,0)
train$x9gt90 <- ifelse(train$x9 > 98,1,0)
train$x9gt95pct <- ifelse(train$x9 > quantile(train$x9,0.95),1,0)
train$x7is0 <- ifelse(train$x7==0,1,0)
train$x7gt90 <- ifelse(train$x7 > 90,1,0)
train$x7gt75pct <- ifelse(train$x7 > quantile(train$x7gt75pct[train$x7gt75pct < 90],0.75),1,0)
train$x397alldpd <-
  ifelse(train$x3 < 90 |
           train$x7 < 90 |
           train$x9 < 90, train$x3 + train$x7 + train$x9, train$x3)
train$x397alldpdis0 <- ifelse(train$x397alldpd==0,1,0)
train$x397alldpdgt90 <- ifelse(train$x397alldpd > 98,1,0)
train$x397alldpdgt95pct <-
  ifelse(train$x397alldpd > quantile(train$x397alldpd[train$x397alldpd < 90],0.95),1,0)
train$x397alldpdgt75pct <-
  ifelse(train$x397alldpd > quantile(train$x397alldpd[train$x397alldpd < 90],0.75),1,0)
train$x10isNA <- ifelse(is.na(train$x10),1,0)
train$x10is0 <- ifelse(train$x10==0,1,0)
train$x10gt95pct <- ifelse(train$x10 >quantile(train$x10,.95, na.rm = TRUE),1,0)
train$x10i <- train$x10
# MEdian value imputation
train$x10i[is.na(train$x10)] <- 0

#x5 Monthly income
train$x5isNULL <- ifelse(is.na(train$x5),1,0)
train$x5is0 <- ifelse(train$x5==0,1,0)
train$x5gt75pct <- ifelse(train$x5 >quantile(train$x5,.75, na.rm = TRUE),1,0)
train$x5i <- train$x5
train$x5i[is.na(train$x5i)] <- median(train$x5, na.rm = TRUE)

#x6: NumberOfOpenCreditLinesAndLoans
train$x6cc <- ifelse(is.finite(train$x6- train$x8),train$x6- train$x8,-9999)
train$x8ge6cc <- ifelse(train$x6cc >= train$x8,1,0)

#x4: DebtRatio
train$x4debt <- train$x4*ifelse(is.na(train$x5)== TRUE,1,train$x5)
train$x4incm.dbt.gap <- ifelse(is.finite(train$x5i- train$x4debt),train$x5i- train$x4debt, -9999)
train$x4debtbyalldpd <- ifelse(is.finite(train$x4debt/train$x397alldpd),train$x4debt/x397alldpd,-9999)
train$x4debtby60_89dpd <- ifelse(is.finite(train$x4debt/train$x7),train$x4debt/x397alldpd,-9999)


#x8: NumberRealEstateLoansOrLines
#train$xt64avgdebt <- ifelse(is.finite(train$x4debt/train$x6) == TRUE, train$x4debt/train$x6, -9999)
#train$xt64avgdebtlog <- ifelse(is.finite(log(train$xt64avgdebt))== TRUE,log(train$xt64avgdebt), -9999 )

print('Executed')
return(train)
# End of fucntion
}