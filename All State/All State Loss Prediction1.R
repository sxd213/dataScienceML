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
 
 tr.data <- df.data[df.data$flgtr.1==1,]
 te.data <- df.data[df.data$flgte.1==1,]
 remove(df.data)
 
 tr.data$flgtr<- NULL
 tr.data$flgte<- NULL
 tr.data$flgtr.1<- NULL
 tr.data$flgte.1<- NULL
 
 save(tr.data, file = 'tr.rda')
 save(te.data, file = 'te.rda')
 remove(tr.data, te.data)
