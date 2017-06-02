memory.limit(size = 24000)

dir <-  'D:\\MyStuff\\Kaggle\\All State'
setwd(dir)


train <- read.csv("train.csv",  header = TRUE, sep = ",", stringsAsFactors = TRUE, row.names = 1, na.strings=c("","NA"))
test <- read.csv("test.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE,row.names = 1,na.strings=c("","NA"))
#test$flg <- NULL

y <- train$loss
save(y, file = 'y.rda')
#load('y.rda')
remove(y)
train$loss <- NULL

train$flg <- 'tr'
test$flg <- 'te'

train$flg <- as.factor(train$flg)

df <- rbind(train,test)
remove(train,test)
# 
# df.na <- names(df)[sapply(df, function(x) length(which(is.na(x))))!=0]
 df.fac <-names(df)[sapply(df, is.factor)]
 #df.fac <- df.fac[1: length(df.fac)-1]
 mm.fac <-
   model.matrix( ~ ., data = df[,df.fac], contrasts.arg = lapply(df[df.fac], contrasts, contrasts =
                                                                   FALSE), na.action='na.pass')
 df.oth<-names(df)[sapply(df, is.numeric)]
 fa.col <- colnames(mm.fac)
 sc.col <- fa.col[1: (length(fa.col)-2)]
 flg.col <- fa.col[(length(fa.col)-1): length(fa.col)]
 scl.mm.fac <- scale(mm.fac[,fa.col], center = TRUE, scale = TRUE)
 scl.mm.fac[,'(Intercept)']<- 1
 scl.mm.fac <- cbind(scl.mm.fac, mm.fac[,flg.col])
 
 mm.oth <- model.matrix(~.,df[df.oth])
 scl.mm.oth <- scale(mm.oth[,-1], center = TRUE, scale = TRUE)
 
 df.mat <- cbind(scl.mm.fac,scl.mm.oth)
 remove(mm.fac, mm.oth)
 remove(scl.mm.fac,scl.mm.oth)
 remove(df)
 
 #mat.col = colnames(df.mat)
 #df.mat[mat.col] = lapply(df.mat[mat.col], function(x){ifelse(is.nan(x), NA,x)} )
 df.data <- data.frame(df.mat)
 remove(df.mat)
 df.na <- names(df.data[is.na(colMeans(df.data))])

 
 # df.faclvlle2<- names(df[,df.fac])[sapply(df[,df.fac], function(x)
#   length(levels(x)))<2]

# Train set 
Y <- train$loss
train$loss <- NULL
train.fac <-names(train)[sapply(train, is.factor)]
#train$flg <- NULL
train.oth<- names(train)[!sapply(train, is.factor)]


scl.mm.fac <- scale(mm.fac, center = TRUE, scale = TRUE)
scl.mm.fac[,'(Intercept)']<- 1
mm.oth <- model.matrix(~.,train[train.oth])
scl.mm.oth <- scale(mm.oth[,-1], center = TRUE, scale = TRUE)
X <- cbind(scl.mm.fac,scl.mm.oth)

Y <- log1p(Y)
 a<-dim(X)
# [1] 188318   1154

remove(train, train.oth, train.fac, scl.mm.oth, scl.mm.fac, mm.fac, mm.oth)



summary(Y)
library(ggplot2)
qplot(log(Y+1), geom="histogram") 
q <- quantile(Y, prob = c(0.1, 0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
quantile(Y,.96)

#train.df$Y <- train.df$Y-1

library(doMC)
core <- detectCores(all.tests = FALSE, logical = TRUE)
registerDoMC(cores=core)

library(glmnet)

glm.m1 = cv.glmnet(
  X, Y, family = "gaussian", alpha = 1, type.measure = "mae", parallel = TRUE)

remove(X,Y)

# Test set
test <- read.csv("test.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE,row.names = 1,na.strings=c("","NA"))
#test$flg <- NULL
test.fac <-names(test)[sapply(test, is.factor)]
te.mm.fac <-
  model.matrix( ~ ., data = test[,test.fac], contrasts.arg = lapply(test[test.fac], contrasts, contrasts =
                                                                      FALSE), na.action='na.pass')

te.scl.mm.fac <- scale(te.mm.fac, center = TRUE, scale = TRUE)
te.scl.mm.fac[,'(Intercept)']<- 1
test.oth<- names(test)[!sapply(test, is.factor)]
te.mm.oth <- model.matrix(~.,test[test.oth])
te.scl.mm.oth <- scale(te.mm.oth[,-1], center = TRUE, scale = TRUE)
tX <- cbind(te.scl.mm.fac,te.scl.mm.oth)
b<- dim(tX)
tpred1 <- predict(glm.m1, newx= tX, type='response',s='lambda.min')

remove(tX,test)

# glm.m2 = cv.glmnet(
#   X, Y, family = "binomial", alpha = 1, type.measure = "auc", parallel = TRUE)
# tprob2 <- predict(glm.m2, newx= tX, type='response',s='lambda.min')\

train.df <- data.frame(cbind(Y,X))
test.df <- data.frame(TX)

