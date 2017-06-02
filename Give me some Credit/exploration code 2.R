dir <- 'D:/MyStuff/Kaggle/Give me some Credit'
load_data(dir)

edit(train[train$x1m >.1 & train$x1m <=.3 & train$x1bt.2.10==1,])

summary(train$x2)

edit(train[train$x10 > 0,])

summary(train$x4)

edit(train[train$x5==0,])

sapply(train, function(x)all(!is.na(x)))
summary(train$x4[is.null(train$x5)])

        