#Single Variable Regression

str(sales1)

lm(train$price~ train$waterfront)


ncol(sales1)
colnames(sales1)

str(sales1)

lapply(sales1, function(x){sum(!is.finite(x))})

#=============================
#Splitting Train versus Test sample
#=============================
smp_size <- floor(0.8 * nrow(mtcars))

## set the seed to make your partition reproductible
set.seed(1)
train_ind <- sample(seq_len(nrow(sales1)), size = smp_size)

train <- sales1[train_ind, ]
test <- sales1[-train_ind, ]

lapply(sales1[train_ind, ], function(x){sum(!is.finite(x))})
lapply(sales1[-train_ind, ], function(x){sum(!is.finite(x))})


# First  single variable regression

sales1$waterfront<- as.integer(sales1$waterfront)

coln <- names(train)
rsq <- seq(1:ncol(train))
voe<- seq(1:ncol(train))
rsq<-seq(1:ncol(train)) 
intr<- seq(1:ncol(train))
for (i in 1:ncol(train)) {
    Formula <- paste0("train$price~",coln[i])
    print(Formula)
    lm(Formula, train)
#   sing.var.reg<-lm(Formula, train)
#   rsq[i]<- summary(sing.var.reg)$r.squared
#   voe[i]<-sqrt(deviance(sing.var.reg)/df.residual(sing.var.reg))
#   intr[i]<- coefficients(sing.var.reg)[1]

  }


sin.var.result <- cbind(coln,rsq,voe,intr)
print(sin.var.result[order(-rsq,-voe),])

# Multiple variable Regression

All.var.reg <- lm(train$price~., train)
summary(All.var.reg)









