library(ggplot2)

dir <- 'D:/MyStuff/Kaggle/Give me some Credit'

load_data <- function(dir)
{
setwd(dir)
train <- read.csv("cs-training.csv",  header = TRUE, sep = ",", row.names = 1)
test <- read.csv("cs-test.csv", header = TRUE, sep = ",", row.names = 1)
train$flg <- "Y"
test$flg <- "N"
data <- rbind(train, test)
names(data) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10','flg')
names(train) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10','flg')
names(test) <- c('y','x1','x2', 'x3','x4','x5','x6','x7','x8','x9','x10','flg')

}

