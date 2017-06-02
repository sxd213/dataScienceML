
dir <- 'D:/MyStuff/Kaggle/Give me some Credit'

setwd(dir)
#test <- read.csv("cs-testing.csv",  header = TRUE, sep = ",", row.names = 1)
test <- read.csv("cs-test.csv", header = TRUE, sep = ",", row.names = 1)


#names(data) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10')
#names(test) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10')
names(test) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10')



#train$y <- as.factor(train$y)

test$x1bt.1.2 <- as.factor(ifelse((test$x1 > 1 & test$x1 <= 2),"Y","N"))
levels(test$x1bt.1.2)
test$x1bt.2.10 <- as.factor(ifelse((test$x1 > 2 & test$x1 <= 10),"Y","N"))
test$x1gt.10 <- as.factor(ifelse((test$x1 > 10),"Y","N"))
test$x1bt.0.1 <- as.factor(ifelse((test$x1 > 0 & test$x1 <= 1),"Y","N"))
test$x1gt.10 <- as.factor(ifelse((test$x1 > 10),"Y","N"))

# test$x1m <- test$x1
# test$x1m <- ifelse((test$x1m > 2 & test$x1m <= 10),test$x1m/10, test$x1m)
# test$x1m <-ifelse((test$x1m > 10 & test$x1m <= 100),test$x1m/100, test$x1m)
# test$x1m <-ifelse((test$x1m > 100 & test$x1m <= 1000),test$x1m/1000, test$x1m)
# test$x1m <-ifelse((test$x1m > 1000 & test$x1m <= 10000),test$x1m/10000, test$x1m)
# test$x1m <-ifelse((test$x1m > 10000 & test$x1m <= 100000),test$x1m/100000, test$x1m)
# test$x1m <-ifelse((test$x1m > 100000 & test$x1m <= 1000000),test$x1m/1000000, test$x1m)

# x2: Age
# test$x2m <- test$x2
# test$x2m[test$x2m > quantile(test$x2m, .95)] <- quantile(test$x2m, .95)
# test$x2m[test$x2m < quantile(test$x2m, .05)] <- quantile(test$x2m, .05)
# test$x2o <-
#   ifelse(test$x2 > quantile(test$x2, .95) &
#            test$x2 < quantile(test$x2, .05),1,0)

# Capped at 95 percentile floored at 5 percentile

#x3: NumberOfTime30-59DaysPastDueNotWorse
#x7:	NumberOfTimes90DaysLate
#x9:	NumberOfTime60-89DaysPastDueNotWorse

test$x397h <- as.factor(ifelse((test$x7 > test$x3+test$x9),"Y","N"))
test$x397m <- as.factor(ifelse((test$x9 > test$x3+test$x7),"Y","N"))
test$x397l <- as.factor(ifelse((test$x3 > test$x7+test$x9),"Y","N"))
test$x3is0 <- as.factor(ifelse(test$x3==0,"Y","N"))
test$x3gt90 <- as.factor(ifelse(test$x3 > 90,"Y","N"))
#test$x3gt95pct <- as.factor(ifelse(test$x3 > quantile(test$x3,0.95),"Y","N"))
test$x9is0 <- as.factor(ifelse(test$x9==0,"Y","N"))
#test$x9gt90 <- as.factor(ifelse(test$x9 > 98,"Y","N"))
#test$x9gt95pct <- as.factor(ifelse(test$x9 > quantile(test$x9,0.95),"Y","N"))
test$x7is0 <- as.factor(ifelse(test$x7==0,"Y","N"))
test$x7gt90 <- as.factor(ifelse(test$x7 > 90,"Y","N"))
#test$x7gt75pct <- as.factor(ifelse(test$x7 > quantile(test$x7[test$x7 < 90],0.75),"Y","N"))
test$x397alldpd <-
  ifelse(test$x3 < 90 |
           test$x7 < 90 |
           test$x9 < 90, test$x3 + test$x7 + test$x9, test$x3)
test$x397alldpdis0 <- as.factor(ifelse(test$x397alldpd==0,"Y","N"))
#test$x397alldpdgt90 <- as.factor(ifelse(test$x397alldpd > 90,"Y","N"))
# test$x397alldpdgt95pct <-
#   as.factor(ifelse(test$x397alldpd > quantile(test$x397alldpd[test$x397alldpd < 90],0.95),1,0))
test$x397alldpdgt75pct <-
  as.factor(ifelse(test$x397alldpd > quantile(test$x397alldpd[test$x397alldpd < 90],0.75),"Y","N"))
#x10: NumberOfDependents
test$x10isNA <- as.factor(ifelse(is.na(test$x10),"Y","N"))
#test$x10gt95pct <- as.factor(ifelse(test$x10 > quantile(test$x10,.95, na.rm = TRUE),1,0))
test$x10i <- test$x10
#test$x10gt75pct <- as.factor(ifelse(test$x10 >quantile(test$x10,.75, na.rm = TRUE),1,0))
# MEdian value imputation
test$x10i[is.na(test$x10)] <- median(test$x10, na.rm = TRUE)
test$x10is0 <- as.factor(ifelse(test$x10i==0 & test$x10isNA =='Y',"Y","N"))

#x5: MonthlyIncome
test$x5i <- test$x5
test$x5isNULL <- as.factor(ifelse(is.na(test$x5),"Y","N"))
test$x5i[is.na(test$x5i)] <- median(test$x5, na.rm = TRUE)
test$x5is0 <- as.factor(ifelse(test$x5i == 0,"Y","N"))

#x4: DebtRatio
test$x4debt <- test$x4*ifelse(is.na(test$x5)== TRUE | test$x5==0,1,test$x5)
#test$x4dbtlog <- log1p(test$x4debt)
test$x4i <- test$x4debt/(test$x5i+1)
#test$x4ilog <- log1p(test$x4i)
test$x4incm.dbt.gap <- test$x5i- test$x4debt
#test$x4scale.incm.dbt.gap <- scale(test$x5i- test$x4debt)

#x6: NumberOfOpenCreditLinesAndLoans
test$x6cc <- test$x6- test$x8
test$x8ge6cc <- as.factor(ifelse(test$x6cc >= test$x8,"Y","N"))

#x8: NumberRealEstateLoansOrLines
test$x6[test$x8==54]
test$x6co <- as.factor(ifelse(test$x4debt > 0 & test$x6==0,"Y","N"))
test$x4avdbt <- test$x4debt/(test$x6+1)
test$x4lavdbt <- log1p(test$x4avdbt)

test.bk <- test

test.na <- names(test)[sapply(test, function(x) length(which(is.na(x))))!=0]
test.fac <-names(test)[sapply(test, is.factor)]
test.fac <- test.fac[test.fac!='y']
test.incmplt <-
  names(test)[sapply(test, function(x)
    length(which(!complete.cases(x))))>0]
test.faclvlle2<- names(test[,test.fac])[sapply(test[,test.fac], function(x)
  length(levels(x)))<2]
sapply(test[,test.fac], function(x) length(levels(x)))
test.fac <- setdiff(test.fac, test.faclvlle2)
test.fac <- setdiff(test.fac, test.incmplt)
test.oth<- names(test)[!sapply(test, is.factor)]
test.oth <- setdiff(test.oth, test.incmplt)
myvars <- c(test.oth, test.fac)
#myvars <- names(test) %in% c("x5", "x10","x9gt90","y") 
mm.fac <-
  model.matrix( ~ ., data = test[,test.fac], contrasts.arg = lapply(test[test.fac], contrasts, contrasts =
                                                                        FALSE), na.action='na.pass')
scl.mm.fac <- scale(mm.fac, center = TRUE, scale = TRUE)
scl.mm.fac[,'(Intercept)']<- 1
mm.oth <- model.matrix(~.,test[test.oth])
scl.mm.oth <- scale(mm.oth[,-1], center = TRUE, scale = TRUE)
tX <- cbind(scl.mm.fac,scl.mm.oth)
#Y <- test$y

test.df <- data.frame(tX)

test.frmla <- as.formula(paste("y ~ ", paste(colnames(X), collapse= "+")))

test.df$X.Intercept.<- NULL

str(test.df)

# for (f in names(test)) {
#   if (class(test[[f]])=="factor") {
#     levels <- unique(c(test[[f]]))
#     test[[f]] <- factor(test[[f]],
#                          labels=make.names(levels))
#   }
# }
test <- test[!myvars]

# test.num <-names(test)[sapply(test, is.numeric)]
# test.int <- names(test)[sapply(test, is.integer)]




#sapply(test, function(x) length(which(!complete.cases(x))))





           

