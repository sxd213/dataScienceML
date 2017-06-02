# Prudential Kaggle Challenge
#libraries
library(ggplot2)
library(lattice)
library(car)
library(R2HTML)
library(caret)
library(dplyr)

########################################################
##Importing Data#####################################
#######################################################

grp_buckets <- function(var,p){
  cut(var, 
      breaks= unique(quantile(var,probs=seq(0,1,by=p), na.rm=T)),
      include.lowest=T, ordered=T) 
}

setwd("D:\\MyStuff\\Kaggle\\Prudential\\Data")

train <- read.csv("train.csv//train.csv",stringsAsFactors = T) #59,381 observations, 128 variables
test <- read.csv("test.csv//test.csv",stringsAsFactors = T) #19,765 observations, 127 variables - test does not have a response field

train$flag <- 'train'
test$flag <- 'test'
test$Response <- 'NA'

fulldata <- rbind(train,test)
nrow(train)
nrow(test)
table(fulldata$Response)
###################################################
### Segregating Char, num, etc fields
###################################################

fea.num <- sapply(fulldata, is.numeric)
fea.int <- sapply(fulldata, is.integer)
fea.char <- sapply(fulldata, is.character)

colnames(fulldata[,fea.char])
ncol(fulldata)

setdiff(colnames(fulldata[,fea.num]), colnames(fulldata[,fea.int]))

###################################################
### Improving data understanding
###################################################

colnames(fulldata)
#[1] "Id"                  
#"Product_Info_1"      "Product_Info_2"      "Product_Info_3"     "Product_Info_4" 
#"Product_Info_5"      "Product_Info_6"      "Product_Info_7" 
# Product_Info_1 is binary. 2 is a minority group
# Product_Info_2 19 categories character
# Product_Info_3 nothing
# Product_Info_4 continuous
# Product_Info_5 2 values tiny proportions of 6 & 8
# Product_Info_6 2 valued. 1 and 3 nothing significant
# Product_Info_7 2 valued.
ggplot(train, aes(x = train$Product_Info_3, fill = as.factor(train$Response)))+
  geom_histogram(binwidth = 1)
table(fulldata$Product_Info_3)
table(cut(train$Product_Info_3, 3, include.lowest = TRUE))
table(cut(train$Product_Info_3, 4, include.lowest = "TRUE", labels = c("PI31", "PI32","PI33","PI34")))
train$Product_Info_3_bin <- cut(train$Product_Info_3, 4, include.lowest = "TRUE", labels = c("PI31", "PI32","PI33","PI34"))
fulldata$Product_Info_3_bin <- cut(fulldata$Product_Info_3, 4, include.lowest = "TRUE", labels = c("PI31", "PI32","PI33","PI34"))


#train$Product_Info_3_qtr <- as.factor(cut(train$Product_Info_3, quantile(train$Product_Info_4, probs=0:4/4), include.lowest=TRUE))
#fulldata$Product_Info_3_qtr <- as.integer(cut(fulldata$Product_Info_3, quantile(fulldata$Product_Info_4, probs=0:4/4), include.lowest=TRUE))

train$Product_Info_3_qtr <- as.factor(ntile(train$Product_Info_3,4))
fulldata$Product_Info_3_qtr <- as.factor(ntile(fulldata$Product_Info_3,4))

table(train$Product_Info_3_qtr,train$Product_Info_3_bin )

train$Product_Info_3_qtr <- as.factor(ntile(train$Product_Info_3,4))
fulldata$Product_Info_3_qtr <- as.factor(ntile(fulldata$Product_Info_3,4))

# Product Info 4
train$Product_Info_4_bin <- cut(train$Product_Info_4, 4, include.lowest = "TRUE", labels = c("PI41", "PI42","PI43","PI44"))
fulldata$Product_Info_4_bin <- cut(fulldata$Product_Info_4, 4, include.lowest = "TRUE", labels = c("PI41", "PI42","PI43","PI44"))

train$Product_Info_4_qtr <- as.factor(ntile(train$Product_Info_4,4))
fulldata$Product_Info_4_qtr <- as.factor(ntile(fulldata$Product_Info_4,4))

table(train$Product_Info_4_qtr,train$Product_Info_4_bin )


ggplot(train, aes(x = train$Product_Info_4, fill = as.factor(train$Response)))+
  geom_histogram(binwidth = .01)

table(cut(train$Product_Info_4, 4, include.lowest = "TRUE", labels = c("PI41", "PI42","PI43","PI44")))

table(fulldata$Product_Info_6)
fulldata$Product_Info_4_qtr <- as.integer(cut(fulldata$Product_Info_4, quantile(fulldata$Product_Info_4, probs=0:4/4), include.lowest=TRUE))
# New Variables PRoduct Info

train$Product_Info <- interaction(train$Product_Info_1, train$Product_Info_2, train$Product_Info_3_bin, train$Product_Info_4_bin, train$Product_Info_5, train$Product_Info_6, train$Product_Info_7, sep = ":")

fulldata$Product_Info <- interaction(fulldata$Product_Info_1, fulldata$Product_Info_2, fulldata$Product_Info_3_bin, fulldata$Product_Info_4_bin, fulldata$Product_Info_5, fulldata$Product_Info_6, fulldata$Product_Info_7, sep = ":")
head(fulldata$Product_Info)

train$Product_Info_int <- interaction(as.factor(ntile(train$Product_Info_1,4)), 
            as.factor(ntile(train$Product_Info_2,4)), 
            as.factor(ntile(train$Product_Info_3,4)),
            as.factor(ntile(train$Product_Info_4,4)),
            as.factor(ntile(train$Product_Info_5,4)),
            as.factor(ntile(train$Product_Info_6,4)),
            as.factor(ntile(train$Product_Info_7,4)),
            sep= ":")

fulldata$Product_Info_int <- interaction(as.factor(ntile(fulldata$Product_Info_1,4)), 
                                      as.factor(ntile(fulldata$Product_Info_2,4)), 
                                      as.factor(ntile(fulldata$Product_Info_3,4)),
                                      as.factor(ntile(fulldata$Product_Info_4,4)),
                                      as.factor(ntile(fulldata$Product_Info_5,4)),
                                      as.factor(ntile(fulldata$Product_Info_6,4)),
                                      as.factor(ntile(fulldata$Product_Info_7,4)),
                                      sep= ":")

summary(train$Product_Info_int)

ggplot(train, aes(x = train$Product_Info_int, fill = as.factor(train$Response)))+
  geom_histogram(binwidth = 1000)




#[9] "Ins_Age"             "Ht"                  "Wt"                  "BMI"   
# Ins_Age need to do proportional study. Lower echelons seem to have lower categories of response variables
# Ht Continuous. 8 to the left; 
# wt2ht looks interesting
table(fulldata$wt2ht)

quantile(fulldata$Ht,0.99)

fulldata <- fulldata1
fulldata$Ht <- sapply(fulldata$Ht, function(x) pmin(0.9090909,x))
fulldata$Ht <- sapply(fulldata$Ht, function(x) pmax(0.5090909,x))

quantile(fulldata$Wt, 0.999) #0.6757322
quantile(fulldata$Wt, .001) #0.1108787
quantile(fulldata$Ht, 0.999) # 0.9090909
quantile(fulldata$Ht, 0.001) # 0.5090909 

summary(fulldata1$Ht)

fulldata$wt2ht <- (fulldata$Wt+.001)/(fulldata$Ht+0.001)
train <- subset(fulldata, flag == 'train')
colnames(train)
nrow(train)
summary(fulldata$wt2ht)
summary(fulldata$Ht)


#[13] "Employment_Info_1"   "Employment_Info_2"   "Employment_Info_3"   "Employment_Info_4"  
#[17] "Employment_Info_5"   "Employment_Info_6"   


range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#_2
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   9.000   9.000   8.642   9.000  38.000
train$Employment_Info_2_sc <- range01(train$Employment_Info_2)
fulldata$Employment_Info_2_sc <- range01(fulldata$Employment_Info_2)
summary(train$Employment_Info_6)
#_5
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.000   2.000   2.000   2.143   2.000   3.000
train$Employment_Info_5_sc <- range01(train$Employment_Info_5)
fulldata$Employment_Info_5_sc <- range01(fulldata$Employment_Info_5)
train$Employment_Info <- train$Employment_Info_1+ train$Employment_Info_2_sc+ 
  train$Employment_Info_3+ train$Employment_Info_4+ train$Employment_Info_5_sc+
  train$Employment_Info_6

train$Employment_Info <- train$Employment_Info_1 + train$Employment_Info_2_sc+
                                       train$Employment_Info_3+ train$Employment_Info_4+ train$Employment_Info_5_sc+ train$Employment_Info_6

ggplot(train, aes(x = train$Employment_Info, fill = as.factor(train$Response)))+
  geom_histogram(binwidth = .01)



#"InsuredInfo_1"       "InsuredInfo_2"      
#[21] "InsuredInfo_3"       "InsuredInfo_4"       "InsuredInfo_5"       "InsuredInfo_6"      
#[25] "InsuredInfo_7"   

table(train$InsuredInfo_7)
ntile(train$InsuredInfo_1,4)

train$InsuredInfo_int <- interaction(as.factor(train$InsuredInfo_1), 
                                      as.factor(train$InsuredInfo_2), 
                                      as.factor(train$InsuredInfo_3),
                                      as.factor(train$InsuredInfo_4),
                                      as.factor(train$InsuredInfo_5),
                                      as.factor(train$Product_Info_6),
                                      as.factor(train$Product_Info_7),
                                      sep= ":")

fulldata$InsuredInfo_int <- interaction(as.factor(fulldata$InsuredInfo_1), 
                                     as.factor(fulldata$InsuredInfo_2), 
                                     as.factor(fulldata$InsuredInfo_3),
                                     as.factor(fulldata$InsuredInfo_4),
                                     as.factor(fulldata$InsuredInfo_5),
                                     as.factor(fulldata$Product_Info_6),
                                     as.factor(fulldata$Product_Info_7),
                                     sep= ":")


ggplot(train, aes(x = train$InsuredInfo_int, fill = as.factor(train$Response)))+
  geom_histogram(binwidth = .5)

#"Insurance_History_1" "Insurance_History_2" "Insurance_History_3"
#[29] "Insurance_History_4" "Insurance_History_5" "Insurance_History_7" "Insurance_History_8"
#[33] "Insurance_History_9" 

train$InsuredHist <- interaction(as.factor(train$Insurance_History_1), 
                                     as.factor(train$Insurance_History_2), 
                                     as.factor(train$Insurance_History_3),
                                     as.factor(train$Insurance_History_4),
                                     as.factor(ntile(train$Insurance_History_5,4)),
                                     as.factor(ntile(train$Insurance_History_7,4)),
                                     as.factor(ntile(train$Insurance_History_8,4)),
                                     sep= ":")

fulldata$InsuredHist <- interaction(as.factor(fulldata$Insurance_History_1), 
                                 as.factor(fulldata$Insurance_History_2), 
                                 as.factor(fulldata$Insurance_History_3),
                                 as.factor(fulldata$Insurance_History_4),
                                 as.factor(fulldata(train$Insurance_History_5,4)),
                                 as.factor(fulldata(train$Insurance_History_7,4)),
                                 as.factor(fulldata(train$Insurance_History_8,4)),
                                 sep= ":")

ggplot(train, aes(x = train$InsuredHist, fill = as.factor(train$Response)))+
  geom_histogram(binwidth = .1)

summary(train$Insurance_History_8)

#"Family_Hist_1"       "Family_Hist_2"       "Family_Hist_3"      
#[37] "Family_Hist_4"       "Family_Hist_5"  

train$Family_Hist <- rowSums(train$Family_Hist_1, 
                             train$Family_Hist_2, 
                             train$Family_Hist_3, 
                             train$Family_Hist_4, 
                             train$Family_Hist_5,)

table(train$Family_Hist_5)

#############--- Good until here

#############--- Good until here


#"Medical_History_1"   "Medical_History_2"  
#[41] "Medical_History_3"   "Medical_History_4"   "Medical_History_5"   "Medical_History_6"  
#[45] "Medical_History_7"   "Medical_History_8"   "Medical_History_9"   "Medical_History_10" 
#[49] "Medical_History_11"  "Medical_History_12"  "Medical_History_13"  "Medical_History_14" 
#[53] "Medical_History_15"  "Medical_History_16"  "Medical_History_17"  "Medical_History_18" 
#[57] "Medical_History_19"  "Medical_History_20"  "Medical_History_21"  "Medical_History_22" 
#[61] "Medical_History_23"  "Medical_History_24"  "Medical_History_25"  "Medical_History_26" 
#[65] "Medical_History_27"  "Medical_History_28"  "Medical_History_29"  "Medical_History_30" 
#[69] "Medical_History_31"  "Medical_History_32"  "Medical_History_33"  "Medical_History_34" 
#[73] "Medical_History_35"  "Medical_History_36"  "Medical_History_37"  "Medical_History_38" 
#[77] "Medical_History_39"  "Medical_History_40"  "Medical_History_41"  

#"Medical_Keyword_1"  
#[81] "Medical_Keyword_2"   "Medical_Keyword_3"   "Medical_Keyword_4"   "Medical_Keyword_5"  
#[85] "Medical_Keyword_6"   "Medical_Keyword_7"   "Medical_Keyword_8"   "Medical_Keyword_9"  
#[89] "Medical_Keyword_10"  "Medical_Keyword_11"  "Medical_Keyword_12"  "Medical_Keyword_13" 
#[93] "Medical_Keyword_14"  "Medical_Keyword_15"  "Medical_Keyword_16"  "Medical_Keyword_17" 
#[97] "Medical_Keyword_18"  "Medical_Keyword_19"  "Medical_Keyword_20"  "Medical_Keyword_21" 
#[101] "Medical_Keyword_22"  "Medical_Keyword_23"  "Medical_Keyword_24"  "Medical_Keyword_25" 
#[105] "Medical_Keyword_26"  "Medical_Keyword_27"  "Medical_Keyword_28"  "Medical_Keyword_29" 
#[109] "Medical_Keyword_30"  "Medical_Keyword_31"  "Medical_Keyword_32"  "Medical_Keyword_33" 
#[113] "Medical_Keyword_34"  "Medical_Keyword_35"  "Medical_Keyword_36"  "Medical_Keyword_37" 
#[117] "Medical_Keyword_38"  "Medical_Keyword_39"  "Medical_Keyword_40"  "Medical_Keyword_41" 
#[121] "Medical_Keyword_42"  "Medical_Keyword_43"  "Medical_Keyword_44"  "Medical_Keyword_45" 
#[125] "Medical_Keyword_46"  "Medical_Keyword_47"  "Medical_Keyword_48"  

ggplot(train, aes(x = train$Number_medical_keywords, fill = as.factor(train$Response)))+
  geom_histogram(binwidth = .1)

fulldata$Number_medical_keywords <- rowSums(fulldata[,(paste("Medical_Keyword_",1:48,sep=""))])

train$Number_medical_keywords <- rowSums(train[,(paste("Medical_Keyword_",1:48,sep=""))])

#"Response"           
#[129] "flag"
#

typeof(fulldata$Employment_Info_6)
summary(fulldata$Employment_Info_6)
# Has a 
hist(train$Employment_Info_6)
table(fulldata$Product_Info_1,fulldata$Product_Info_5)

table(cut(train$Product_Info_6, 3, include.lowest = TRUE))

table(cut(train$Product_Info_6, 3, include.lowest = "TRUE", labels = c("low", "medium", "high")))
summary(train$Employment_Info_6)


ggplot(train, aes(x = train$Product_Info_5, y = train$Product_Info_6))+
  geom_point(aes(colors= train$Response))+
  geom_jitter()

fulldata$Product_Info_6_qtr <- as.integer(cut(fulldata$Product_Info_6, quantile(fulldata$Product_Info_6, probs=0:4/4), include.lowest=TRUE))


table(paste(train$Product_Info_1,train$Product_Info_2,train$Product_Info_3, train$Product_Info_4, train$Product_Info_5, train$Product_Info_6, train$Product_Info_7 ))
table(train$Product_Info_2, train$Product_Info_4)


ggplot(train, aes(x = grp_buckets(train$Product_Info_4,.25), fill = as.factor(train$Response)))+
  geom_histogram(binwidth = .01)


# Employment Info 2:- What to make out of it? Looks like ternary code
# Employment_Info_3:- Looks binary
table(train$Employment_Info_4, train$Response)
ggplot(train, aes(x = train$Employment_Info_4, fill = as.factor(train$Response)))+
  geom_histogram(binwidth = .005)
quantile(fulldata$Product_Info_4, 0.05)

# BMI 8 and 6 are different end of the spectrum

binwidth = 0.1
ggplot(train, aes(x = train$BMI, fill = train$Response))+
  geom_bar(binwidth = 0.01)




head(train,1)
