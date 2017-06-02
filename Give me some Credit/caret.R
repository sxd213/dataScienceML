library(C50)
data(churn)
str(churnTrain)
predictors <- names(churnTrain)[names(churnTrain) != "churn"]

library(gbm)
forGBM <- churnTrain
forGBM$churn <- ifelse(forGBM$churn =='yes',1,0)

gbmFit <- gbm(formula = churn~.,
              distribution = 'bernoulli',
              data = forGBM,
              n.trees = 2000,
              interaction.depth = 7,
              shrinkage = 0.001,
              verbose = FALSE)