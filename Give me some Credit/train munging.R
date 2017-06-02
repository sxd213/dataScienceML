
dir <- 'D:/MyStuff/Kaggle/Give me some Credit'

setwd(dir)
train <- read.csv("cs-training.csv",  header = TRUE, sep = ",", row.names = 1)
test <- read.csv("cs-test.csv", header = TRUE, sep = ",", row.names = 1)


#names(data) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10')
names(train) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10')
names(test) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10')

train$y <- as.factor(train$y)

train$x1bt.1.2 <- as.factor(ifelse((train$x1 > 1 & train$x1 <= 2),1,0))
train$x1bt.2.10 <- as.factor(ifelse((train$x1 > 2 & train$x1 <= 10),1,0))
train$x1gt.10 <- as.factor(ifelse((train$x1 > 10),1,0))
train$x1bt.0.1 <- as.factor(ifelse((train$x1 > 0 & train$x1 <= 1),1,0))
train$x1gt.10 <- as.factor(ifelse((train$x1 > 10),1,0))

# train$x1m <- train$x1
# train$x1m <- ifelse((train$x1m > 2 & train$x1m <= 10),train$x1m/10, train$x1m)
# train$x1m <-ifelse((train$x1m > 10 & train$x1m <= 100),train$x1m/100, train$x1m)
# train$x1m <-ifelse((train$x1m > 100 & train$x1m <= 1000),train$x1m/1000, train$x1m)
# train$x1m <-ifelse((train$x1m > 1000 & train$x1m <= 10000),train$x1m/10000, train$x1m)
# train$x1m <-ifelse((train$x1m > 10000 & train$x1m <= 100000),train$x1m/100000, train$x1m)
# train$x1m <-ifelse((train$x1m > 100000 & train$x1m <= 1000000),train$x1m/1000000, train$x1m)

# x2: Age
train$x2m <- train$x2
train$x2m[train$x2m > quantile(train$x2m, .95)] <- quantile(train$x2m, .95)
train$x2m[train$x2m < quantile(train$x2m, .05)] <- quantile(train$x2m, .05)
train$x2o <-
  ifelse(train$x2 > quantile(train$x2, .95) &
           train$x2 < quantile(train$x2, .05),1,0)

# Capped at 95 percentile floored at 5 percentile

#x3: NumberOfTime30-59DaysPastDueNotWorse
#x7:	NumberOfTimes90DaysLate
#x9:	NumberOfTime60-89DaysPastDueNotWorse

train$x397h <- as.factor(ifelse((train$x7 > train$x3+train$x9),1,0))
train$x397m <- as.factor(ifelse((train$x9 > train$x3+train$x7),1,0))
train$x397l <- as.factor(ifelse((train$x3 > train$x7+train$x9),1,0))
train$x3is0 <- as.factor(ifelse(train$x3==0,1,0))
train$x3gt90 <- as.factor(ifelse(train$x3 > 90,1,0))
#train$x3gt95pct <- as.factor(ifelse(train$x3 > quantile(train$x3,0.95),1,0))
train$x9is0 <- as.factor(ifelse(train$x9==0,1,0))
train$x9gt90 <- as.factor(ifelse(train$x9 > 98,1,0))
#train$x9gt95pct <- as.factor(ifelse(train$x9 > quantile(train$x9,0.95),1,0))
train$x7is0 <- as.factor(ifelse(train$x7==0,1,0))
train$x7gt90 <- as.factor(ifelse(train$x7 > 90,1,0))
#train$x7gt75pct <- as.factor(ifelse(train$x7 > quantile(train$x7[train$x7 < 90],0.75),1,0))
train$x397alldpd <-
  ifelse(train$x3 < 90 |
           train$x7 < 90 |
           train$x9 < 90, train$x3 + train$x7 + train$x9, train$x3)
train$x397alldpdis0 <- as.factor(ifelse(train$x397alldpd==0,1,0))
train$x397alldpdgt90 <- as.factor(ifelse(train$x397alldpd > 90,1,0))
# train$x397alldpdgt95pct <-
#   as.factor(ifelse(train$x397alldpd > quantile(train$x397alldpd[train$x397alldpd < 90],0.95),1,0))
train$x397alldpdgt75pct <-
  as.factor(ifelse(train$x397alldpd > quantile(train$x397alldpd[train$x397alldpd < 90],0.75),1,0))
#x10: NumberOfDependents
train$x10isNA <- as.factor(ifelse(is.na(train$x10),1,0))
#train$x10gt95pct <- as.factor(ifelse(train$x10 > quantile(train$x10,.95, na.rm = TRUE),1,0))
train$x10i <- train$x10
#train$x10gt75pct <- as.factor(ifelse(train$x10 >quantile(train$x10,.75, na.rm = TRUE),1,0))
# MEdian value imputation
train$x10i[is.na(train$x10)] <- 0
train$x10is0 <- as.factor(ifelse(train$x10i==0 & train$x10isNA ==0,1,0))

#x5: MonthlyIncome
train$x5i <- train$x5
train$x5isNULL <- as.factor(ifelse(is.na(train$x5),1,0))
train$x5i[is.na(train$x5i)] <- median(train$x5, na.rm = TRUE)
train$x5is0 <- as.factor(ifelse(train$x5i == 0,1,0))

#x4: DebtRatio
train$x4debt <- train$x4*ifelse(is.na(train$x5)== TRUE | train$x5==0,1,train$x5)
train$x4dbtlog <- log1p(train$x4debt)
train$x4i <- train$x4debt/(train$x5i+1)
train$x4ilog <- log1p(train$x4i)
train$x4incm.dbt.gap <- train$x5i- train$x4debt
train$x4scale.incm.dbt.gap <- scale(train$x5i- train$x4debt)

#x6: NumberOfOpenCreditLinesAndLoans
train$x6cc <- train$x6- train$x8
train$x8ge6cc <- as.factor(ifelse(train$x6cc >= train$x8,1,0))

#x8: NumberRealEstateLoansOrLines
train$x6[train$x8==54]
train$x6co <- as.factor(ifelse(train$x4debt > 0 & train$x6==0,1,0))
train$x4avdbt <- train$x4debt/(train$x6+1)
train$x4lavdbt <- log1p(train$x4avdbt)

sapply(train, function(x) length(which(!is.finite(x))))

train.bk <- train
y <- train$y
myvars <- names(train) %in% c("x5", "x10","x9gt90","y") 
myvars.gbm <- names(train) %in% c("x5", "x10","x9gt90")
train.gbm <- train[!myvars.gbm]

feature.names=names(train.gbm)

for (f in feature.names) {
  if (class(train[[f]])=="factor") {
    levels <- unique(c(train[[f]]))
    train[[f]] <- factor(train[[f]],
                         labels=make.names(levels))
  }
}
train <- train[!myvars]
train.fac <-names(train)[sapply(train, is.factor)]
train.num <-names(train)[sapply(train, is.numeric)]
train.int <- names(train)[sapply(train, is.integer)]
train.oth<- names(train)[!sapply(train, is.factor)]

myvars

sapply(train[,train.fac], function(x) length(which(!complete.cases(x))))

sapply(train, function(x) length(which(!complete.cases(x))))


mm.fac <-
  model.matrix( ~ ., data = train[,train.fac], contrasts.arg = lapply(train[train.fac], contrasts, contrasts =
                                                            FALSE), na.action='na.pass')


mm.oth <- model.matrix(~.,train[train.oth])

x <- cbind(mm.fac,mm.oth)
           

