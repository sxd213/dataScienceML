
test$x1bt.1.2 <- as.factor(ifelse((test$x1 > 1 & test$x1 <= 2),1,0))
test$x1bt.2.10 <- as.factor(ifelse((test$x1 > 2 & test$x1 <= 10),1,0))
test$x1gt.10 <- as.factor(ifelse((test$x1 > 10),1,0))
test$x1bt.0.1 <- as.factor(ifelse((test$x1 > 0 & test$x1 <= 1),1,0))

test$x1m <- test$x1
test$x1m <- ifelse((test$x1m > 2 & test$x1m <= 10),test$x1m/10, test$x1m)
test$x1m <-ifelse((test$x1m > 10 & test$x1m <= 100),test$x1m/100, test$x1m)
test$x1m <-ifelse((test$x1m > 100 & test$x1m <= 1000),test$x1m/1000, test$x1m)
test$x1m <-ifelse((test$x1m > 1000 & test$x1m <= 10000),test$x1m/10000, test$x1m)
test$x1m <-ifelse((test$x1m > 10000 & test$x1m <= 100000),test$x1m/100000, test$x1m)
test$x1m <-ifelse((test$x1m > 100000 & test$x1m <= 1000000),test$x1m/1000000, test$x1m)

# x2: Age
test$x2m <- test$x2
test$x2m[test$x2m > quantile(test$x2m, .95)] <- quantile(test$x2m, .95)
test$x2m[test$x2m < quantile(test$x2m, .05)] <- quantile(test$x2m, .05)
test$x2o <-
  ifelse(test$x2 > quantile(test$x2, .95) &
           test$x2 < quantile(test$x2, .05),1,0)

# Capped at 95 percentile floored at 5 percentile

#x3: NumberOfTime30-59DaysPastDueNotWorse
#x7:	NumberOfTimes90DaysLate
#x9:	NumberOfTime60-89DaysPastDueNotWorse

test$x397h <- as.factor(ifelse((test$x7 > test$x3+test$x9),1,0))
test$x397m <- as.factor(ifelse((test$x9 > test$x3+test$x7),1,0))
test$x397l <- as.factor(ifelse((test$x3 > test$x7+test$x9),1,0))
test$x3is0 <- as.factor(ifelse(test$x3==0,1,0))
test$x3gt90 <- as.factor(ifelse(test$x3 > 90,1,0))
test$x3gt95pct <- as.factor(ifelse(test$x3 > quantile(test$x3,0.95),1,0))
test$x9is0 <- as.factor(ifelse(test$x9==0,1,0))
test$x9gt90 <- as.factor(ifelse(test$x9 > 98,1,0))
test$x9gt95pct <- as.factor(ifelse(test$x9 > quantile(test$x9,0.95),1,0))
test$x7is0 <- as.factor(ifelse(test$x7==0,1,0))
test$x7gt90 <- as.factor(ifelse(test$x7 > 90,1,0))
test$x7gt75pct <- as.factor(ifelse(test$x7 > quantile(test$x7[test$x7 < 90],0.75),1,0))
test$x397alldpd <-
  ifelse(test$x3 < 90 |
           test$x7 < 90 |
           test$x9 < 90, test$x3 + test$x7 + test$x9, test$x3)
test$x397alldpdis0 <- as.factor(ifelse(test$x397alldpd==0,1,0))
test$x397alldpdgt90 <- as.factor(ifelse(test$x397alldpd > 90,1,0))
test$x397alldpdgt95pct <-
  as.factor(ifelse(test$x397alldpd > quantile(test$x397alldpd[test$x397alldpd < 90],0.95),1,0))
test$x397alldpdgt75pct <-
  as.factor(ifelse(test$x397alldpd > quantile(test$x397alldpd[test$x397alldpd < 90],0.75),1,0))
#x10: NumberOfDependents
test$x10isNA <- as.factor(ifelse(is.na(test$x10),1,0))
test$x10is0 <- as.factor(ifelse(test$x10==0,1,0))
#test$x10gt95pct <- as.factor(ifelse(test$x10 > quantile(test$x10,.95, na.rm = TRUE),1,0))
test$x10i <- test$x10
#test$x10gt75pct <- as.factor(ifelse(test$x10 >quantile(test$x10,.75, na.rm = TRUE),1,0))
# MEdian value imputation
test$x10i[is.na(test$x10)] <- 0

#x5: MonthlyIncome
test$x5i <- test$x5
test$x5isNULL <- as.factor(ifelse(is.na(test$x5),1,0))
test$x5i[is.na(test$x5i)] <- median(test$x5, na.rm = TRUE)
test$x5is0 <- as.factor(ifelse(test$x5i == 0,1,0))

#x4: DebtRatio
test$x4debt <- test$x4*ifelse(is.na(test$x5)== TRUE | test$x5==0,1,test$x5)
test$x4dbtlog <- log1p(test$x4debt)
test$x4i <- test$x4debt/(test$x5i+1)
test$x4ilog <- log1p(test$x4i)
test$x4incm.dbt.gap <- test$x5i- test$x4debt
test$x4scale.incm.dbt.gap <- scale(test$x5i- test$x4debt)

#x6: NumberOfOpenCreditLinesAndLoans
test$x6cc <- test$x6- test$x8
test$x8ge6cc <- as.factor(ifelse(test$x6cc >= test$x8,1,0))

#x8: NumberRealEstateLoansOrLines
test$x6[test$x8==54]
test$x6co <- as.factor(ifelse(test$x4debt > 0 & test$x6==0,1,0))
test$x4avdbt <- test$x4debt/(test$x6+1)
test$x4lavdbt <- log1p(test$x4avdbt)

sapply(test, function(x) length(which(!is.finite(x))))



test.bk <- test
myvars <- names(test) %in% c("x5", "x10") 
test <- test[!myvars]