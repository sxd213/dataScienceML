library(mlbench)
library(caret)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  repeats =5,
  classProbs = TRUE)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)


set.seed(825)
gbmFit2 <- train(y ~ ., data = train.gbm, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)