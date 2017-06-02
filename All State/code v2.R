

library(doMC)
core <- detectCores(all.tests = FALSE, logical = TRUE)
registerDoMC(cores=core)

load('y.rda')
y <- log1p(y)
load('tr.rda')

gbm.frmla <- as.formula(paste("y ~ ", paste(colnames(tr.data), collapse= "+")))



library(gbm)


gbm.m1 <- gbm(
  formula = gbm.frmla,
  distribution = 'laplace',
  data = tr.data,
  n.trees = 100,
  interaction.depth = 1,
  n.minobsinnode = 10000,
  shrinkage = 0.05,
  bag.fraction = 0.5,
  train.fraction = 0.5,
  cv.folds = 2,
  keep.data = FALSE,
  verbose = TRUE,
  class.stratify.cv = NULL,
  n.cores = 4
)

save(gbm.m1, file='gbm.rda')

remove(tr.data)
load('tr.rda')

summary(y)
tr.data$y <-y
tr.data$wt <- y/(max(y)-min(y))
summary(tr.data$wt)


tr.data$pred <- predict(gbm.m1, tr.data, type='response')
tr.data$pred <- expm1(tr.data$pred)
tr.data$y <-expm1(y)

tr.data$ae <- abs(tr.data$pred- tr.data$y)
mae<- mean(tr.data$ae)

tr.data$wt <- tr.data$ae/mae
tr.data$wt <- tr.data$wt +0.001

tr.data$ae <- NULL
tr.data$y <- NULL
tr.data$pred <- NULL


loss <- predict(gbm.m1, te.data, type='response')
loss <- expm1(loss)
sub<- read.csv("sample_submission.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE,na.strings=c("","NA"))
sub$loss <- loss
names(loss.df)<- c('Id','loss')
write.csv(sub, file = "loss.csv", row.names = FALSE)

remove(sub)
remove(te.data)

gbm.m3 <- gbm(
  formula = gbm.frmla,
  distribution = 'mse',
  data = tr.data,
  weights = wt,
  n.trees = 100,
  interaction.depth = 1,
  n.minobsinnode = 10000,
  shrinkage = 0.05,
  bag.fraction = 0.5,
  train.fraction = 0.5,
  cv.folds = 2,
  keep.data = FALSE,
  verbose = TRUE,
  class.stratify.cv = NULL,
  n.cores = 4
)

load('te.rda')
loss <- predict(gbm.m2, te.data, type='response')
loss <- expm1(loss)
sub<- read.csv("sample_submission.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE,na.strings=c("","NA"))
sub$loss <- loss
names(loss.df)<- c('Id','loss')
write.csv(sub, file = "loss.csv", row.names = FALSE)

