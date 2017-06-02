# Prudential Kaggle Challenge
#libraries
library(ggplot2)
library(lattice)
library(car)
library(R2HTML)
library(caret)


# Import of data and initial rows check
setwd("D:\\MyStuff\\Kaggle\\Prudential\\Data")
getwd()

prutrain <- read.csv("train.csv//train.csv",stringsAsFactors = T) #59,381 observations, 128 variables
prutest <- read.csv("test.csv//test.csv",stringsAsFactors = T) #19,765 observations, 127 variables - test does not have a response field

prutrain$dflg = "train"
prutest$dflg = "test"
prutest$Response = NA


nrow(prutrain)
names(prutest)
nrow(prutest)
typeof(prutrain)
sapply(prutrain, typeof)

# Concatenate All Data into one dataframe

Alldata <- rbind(prutrain, prutest)

#Define variables as either numeric or factor, Data_1 - Numeric Variables, Data_2 - factor variables
Data_1 <- Alldata[,names(Alldata) %in% c("Product_Info_4",	"Ins_Age",	"Ht",	"Wt",	"BMI",	"Employment_Info_1",	"Employment_Info_4",	"Employment_Info_6",	"Insurance_History_5",	"Family_Hist_2",	"Family_Hist_3",	"Family_Hist_4",	"Family_Hist_5",	"Medical_History_1",	"Medical_History_15",	"Medical_History_24",	"Medical_History_32",paste("Medical_Keyword_",1:48,sep=""))]
Data_2 <- Alldata[,!(names(Alldata) %in% c("Product_Info_4",	"Ins_Age",	"Ht",	"Wt",	"BMI",	"Employment_Info_1",	"Employment_Info_4",	"Employment_Info_6",	"Insurance_History_5",	"Family_Hist_2",	"Family_Hist_3",	"Family_Hist_4",	"Family_Hist_5",	"Medical_History_1",	"Medical_History_15",	"Medical_History_24",	"Medical_History_32",paste("Medical_Keyword_",1:48,sep="")))]
Data_2<- data.frame(apply(Data_2, 2, as.factor))

Alldata <- cbind(Data_1, Data_2)

str(Alldata)

##############################################################
#Step 2: Feature Creation - create some features which we want to test in a predictive model
##########################################################
grp_buckets <- function(var,p){
  cut(var, 
      breaks= unique(quantile(var,probs=seq(0,1,by=p), na.rm=T)),
      include.lowest=T, ordered=T) 
}

#Investigate the Wt variable - Normalized weight of applicant
summary(Alldata$Wt)
ggplot(Alldata, aes(Alldata$Response, Alldata$Wt)) + geom_boxplot() +
  ylab("Sample Weight") +
  xlab("Response Variable") 

hist(Alldata$Wt)

#Make a new variable which is equivalent to the quintile groups of Wt, we can use the group_into_buckets function we defined above

Alldata$Wt_quintile <- grp_buckets(Alldata$Wt,0.2)
tq <- table(Alldata$Wt_quintile)
typeof(tq)

class(Alldata$Wt_quintile)
#"ordered" "factor"

Alldata$Ht <- sapply(Alldata$Ht, function(x) pmin(0.9090909,x))
Alldata$Ht <- sapply(Alldata$Ht, function(x) pmax(0.5090909,x))
Alldata$wt2ht <- (Alldata$Wt+.001)/(Alldata$Ht+0.001)

Alldata$Number_medical_keywords <- rowSums(Alldata[,(paste("Medical_Keyword_",1:48,sep=""))])


table(Alldata$Number_medical_keywords)

#There seems to be low frequencies in the higher numbers, depending on the model we may want to cap this

Alldata$Number_medical_keywords <- ifelse(Alldata$Number_medical_keywords>7,7,Alldata$Number_medical_keywords)
table(Alldata$Number_medical_keywords)

#    0     1     2     3     4     5      6     7 
#  31247 21430 12573  7046  3652  1793   796   609 

table(as.numeric(as.factor(Alldata$Product_Info_6)))

table(Alldata$Product_Info)

head(as.numeric(as.factor(Alldata$Product_Info_5))*10**8
+ as.numeric(as.factor(Alldata$Product_Info_2))*10**6
+ as.numeric(as.factor(Alldata$Product_Info_3))*10**4 
+ as.numeric(as.factor(Alldata$Product_Info_4)))

Alldata$Product_Info <- rowSums(as.numeric(as.factor(Alldata$Product_Info_3))*10**4,
                        as.numeric(as.factor(Alldata$Product_Info_4)


ggplot(train, aes(x = train$Product_Info_1, fill = as.factor(train$Response)))+
  geom_histogram(binwidth = 1)

##############################################################
#Step 2a: My own exploration  
##########################################################
# Medical History_2 is a categorical variable

table(Alldata$BMI_quintile)
Alldata$BMI_dec <- grp_buckets(Alldata$BMI,0.1)
hist(Alldata$BMI_dec)

boxplot(Alldata$BMI_dec)

clrcd <- function(inpi)
  (  ifelse(prutrain$Response == inpi,"chartreuse3","red3")
     
    )

ggplot(prutrain, aes(y = prutrain$Ins_Age, x = prutrain$BMI, color = clrcd(2), shape = clrcd(2))) +
  geom_jitter()

#---------------------------------------------
# Variable wise training to difure out what variables to choose
#---------------------------------------------
train <- Alldata[Alldata$dflg=='train',] #59,381, 131 variables
test <- Alldata[Alldata$dflg=='test',] #19,765, 131 variables

response= factor(train$Response)
typeof(response)
scores.grid <- expand.grid(columns = 1:ncol(test), score = NA)


feat = names(test)[20]
temp = data.frame(train[,feat])
typeof(temp[,1])
names(temp) = feat
nf  <- trainControl(method="lm", number=5, classProbs = FALSE, summaryFunction = defaultSummary)
train(x=temp, y=response, method = "lm", metric="RSME",  trControl=nf)

str(train)
##############################################################
#Step 3: Now that we are finished with feature creation lets recreate train and test
##########################################################



set.seed(1234)
train$random <- runif(nrow(train))

##############################################################
#Step 4: Model building - Build a GBM on a random 70% of train and validate on the other 30% of train.
#        This will be an iterative process where you should add/refine/remove features
##########################################################


train_70 <- train[train$random <= 0.7,] #41,561 obs
train_30 <- train[train$random > 0.7,] #17,820 obs

nrow(train_70)

#Lets have a look at distribution of response on train_70 and train_30

round(table(train_70$Response)/nrow(train_70),2)
# 1     2     3      4     5     6    7    8  
#0.10  0.11  0.02  0.02  0.09  0.19  0.13  0.33

round(table(train_30$Response)/nrow(train_30),2)
#  1     2     3     4     5     6     7    8 
#0.10  0.11  0.02  0.02  0.09  0.19  0.14  0.33


#The response distribtion holds up well across the random split

#Lets build a very simple GBM on train_70 and calculate the performance on train_30

#To make the GBM run faster I will subset train_70 to only include the variables I want in the model
train_70 <- train_70[,c("Response","BMI","Wt","Ht","Ins_Age","Number_medical_keywords","Wt_quintile")] #41,561, 7 variables


library("gbm") #cran version, we will use multinomial distribution for a quick solution to this problem


GBM_train <- gbm(Response ~ .,
                 data=train_70,
                 n.trees=50,
                 distribution = "multinomial",
                 interaction.depth=5,
                 n.minobsinnode=40,
                 shrinkage=0.1,
                 cv.folds=0,
                 n.cores=1,
                 train.fraction=1,
                 bag.fraction=0.2,
                 verbose=T)

GBM_train$opt_tree <- gbm.perf(GBM_train, method="OOB") #Use the OOB method to determine the optimal number of trees
summary(GBM_train,n.trees=GBM_train$opt_tree)

Prediction_Object <- predict(GBM_train,train_30,GBM_train$opt_tree,type="response")

train_30$Prediction <- apply(Prediction_Object, 1, which.max)

round(table(train_30$Prediction)/nrow(train_30),2)

round((table(train_30$Prediction,train_30$Response)/nrow(train_30))*100,1)

library("Metrics")
ScoreQuadraticWeightedKappa(train_30$Prediction,as.numeric(train_30$Response))

Prediction_Object <- predict(GBM_train,test,GBM_train$opt_tree,type="response")

test$Response <- apply(Prediction_Object, 1, which.max)

round(table(test$Response)/nrow(test),2)

submission_file <- test[,c("Id","Response")] #19,765 obs, 2 variables

write.csv(submission_file,"Submission_file.csv",row.names=FALSE)


# html output

library(R2PPT)
HTMLStart(outdir="D:\\MyStuff\\Innocentive\\Challenge ID 9933493\\InnoCentive_Challenge_9933493_training_data\\html", file="myreport",
          extension="html", echo=FALSE, HTMLframe=TRUE)
HTML.title("My Report", HR=1)

HTML.title("Description of my data", HR=3)
summary(FullTrain[,6]) 

HTMLhr()

HTML.title("X Y Scatter Plot", HR=2)
plot(FullTrain[,6]~FullTrain[,52])
HTMLplot() 

HTMLStop()




