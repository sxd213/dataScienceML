## Plain simple logistic regression with regulaization
library(doMC)
core <- detectCores(all.tests = FALSE, logical = TRUE)
registerDoMC(cores=core)

library(glmnet)

# a=0
# i=1
# lmbda<-as.numeric(rep(0,10))
# alpha<-as.numeric(rep(0,10))
# auc<-as.numeric(rep(0,10))
# while (a <= 1) {
#   print(c('Now Running alpha:',a))
#   cvfit = cv.glmnet(
#     X, Y, family = "binomial", alpha = a, type.measure = "auc", parallel = TRUE)
#   prob <- predict(cvfit, newx= X, type='response',s='lambda.min')
#   lmbda[i] <- cvfit$lambda.min
#   alpha[i] <- a
#   auc[i]<- getROC_AUC(prob, as.double(Y))
#   i = i+1
#   a = a+0.1
# }

glm.m1 = cv.glmnet(
  X, Y, family = "binomial", alpha = 0, type.measure = "auc", parallel = TRUE)
tprob1 <- predict(glm.m1, newx= tX, type='response',s='lambda.min')

glm.m2 = cv.glmnet(
  X, Y, family = "binomial", alpha = 1, type.measure = "auc", parallel = TRUE)
tprob2 <- predict(glm.m2, newx= tX, type='response',s='lambda.min')

tprob2.df <- data.frame(rownames(tprob2), tprob2)
names(tprob2.df)<- c('Id','Probability')
write.csv(tprob2.df, file = "t2.csv", row.names = FALSE)




library(gbm)

y<- as.factor(train.df$y)
train.gbm <- train.df
train.gbm$y <- NULL
gbm.frmla <- as.formula(paste("y ~ ", paste(colnames(train.gbm), collapse= "+")))

gbm.m1 <- gbm(
  formula = gbm.frmla,
  distribution = "bernoulli",
  data = train.df,
  n.trees = 1000,
  interaction.depth = 3,
  n.minobsinnode = 100,
  shrinkage = 0.05,
  bag.fraction = 0.5,
  train.fraction = 0.5,
  cv.folds = 5,
  keep.data = TRUE,
  verbose = "CV",
  class.stratify.cv = NULL,
  n.cores = 2
)

tprob3 <- predict(gbm.m1, test.df, type='response')

tprob3.df <- data.frame(seq(1:length(tprob3)), tprob3)
names(tprob3.df)<- c('Id','Probability')
write.csv(tprob3.df, file = "t3.csv", row.names = FALSE)

wt <- ifelse(y ==1,3,1)
gbm.m2 <- gbm(
  formula = gbm.frmla,
  distribution = "bernoulli",
  data = train.df,
  weights = wt,
  n.trees = 2000,
  interaction.depth = 3,
  n.minobsinnode = 100,
  shrinkage = 0.05,
  bag.fraction = 0.5,
  train.fraction = 0.5,
  cv.folds = 3,
  keep.data = TRUE,
  verbose = "CV",
  class.stratify.cv = NULL,
  n.cores = 4
)

tprob4 <- predict(gbm.m2, test.df, type='response')

tprob4.df <- data.frame(seq(1:length(tprob4)), tprob4)
names(tprob4.df)<- c('Id','Probability')
write.csv(tprob4.df, file = "t4.csv", row.names = FALSE)

summary(gbm.m2,
        cBars=length(object$var.names),
        n.trees=object$n.trees,
        plotit=TRUE,
        order=TRUE,
        method=relative.influence,
        normalize=TRUE)




library(randomForest)
tuneRF(train.df,y, mtryStart=7, ntreeTry=50, stepFactor=2, improve=0.05,
       trace=TRUE, plot=TRUE, doBest=TRUE)


rf.m1 <- randomForest(gbm.frmla, data=train.df, ntree=1000,
                          keep.forest=FALSE, importance=TRUE)

