dir <- 'D:/MyStuff/Kaggle/Titanic- New Attempt'

setwd(dir)
ttr <- read.csv("train.csv",  header = TRUE, sep = ",", stringsAsFactors = FALSE, row.names = 1, na.strings=c("","NA"))
tte <- read.csv("test.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE,row.names = 1,na.strings=c("","NA"))

tte$Survived  <- 2

ttr$flg <- 'ttr'
tte$flg <- 'tte'

combi<- rbind(ttr,tte)

summary(ttr)

# survival        Survival
#y <- as.factor(ttr$Survived)
# (0 = No; 1 = Yes)

# pclass          Passenger Class

combi$Pclass <- as.factor(combi$Pclass)

# (1 = 1st; 2 = 2nd; 3 = 3rd)[1]
# name            Name
# getTitle <- function(data) {
#   title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
#   title.comma.end <- title.dot.start 
#   + acombi(title.dot.start, "match.length")-1
#   data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
#   return (data$Title)
# }
# combi$Title <- getTitle(combi)

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Capt','Col','Don','Dr','Major','Sir','Jonkheer','Rev')] <- 'Lord'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
combi$Title[combi$Title %in% c('Mme', 'Mrs')] <- 'Mrs'
combi$Title[combi$Title %in% c('Mlle', 'Ms')] <- 'Miss'
table(combi$Title, combi$Survived)
combi$Title <- as.factor(combi$Title)

t <- 'Johnson, Mrs. Oscar W (Elisabeth Vilhelmina Berg)'
strsplit(t, split='[,.]')

# sex             Sex
table(combi$Sex, combi$Survived)
combi$Sex <- as.factor(combi$Sex)
summary(combi$Sex)
# age             Age
summary(combi$Age)

library(plyr)
agem <-ddply(combi,~Title,summarise,median=median(Age, na.rm = TRUE),sd=sd(Age, na.rm = TRUE))
combi$Age <-ifelse(is.na(combi$Age)== TRUE,agem$median[match(combi$Title, agem$Title)],combi$Age)
summary(combi$Age)
breaks <- c(0,5,10,19,35,50,80)
l<- c('infant','children','teen','young adult','adult','old')
combi$agegrp <- cut(combi$Age, breaks, labels= l)
table(combi$agegrp)

nrow(is.null(combi$agegrp)== TRUE)
#titles.na.combi <- levels(combi$Title)

# library(Hmisc)
# imputeMedian <- function(impute.var, filter.var, var.levels) {
#   for (v in var.levels) {
#     impute.var[ which( filter.var == v)] <- median(impute.var[ 
#       which( filter.var == v)])
#   }
#   return (impute.var)
# }
# 
# combi$Age <- imputeMedian(combi$Age, combi$Title, 
#                              titles.na.combi)


# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$SibSp.flg <- ifelse(combi$SibSp!=0,'Y','N')
combi$Parch.flg <- ifelse(combi$Parch!=0, 'Y', 'N')
combi$Parch.flg <- as.factor(combi$SibSp.flg)
combi$SibSp.flg <- as.factor(combi$SibSp.flg)

# nrow(combi[combi$FamilySize!=1,])
# #519
# nrow(combi[combi$SibSp!=0,])
# #418-209
# nrow(combi[combi$Parch!=0,])
# #307
# nrow(combi[combi$SibSp!=0 | combi$Parch!=0,])
# table(as.factor(combi$SibSp))
# table(as.factor(combi$SibSp),as.factor(combi$Parch))
# table(as.factor(combi$FamilySize))
combi$FamName <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
#table(combi$FamName, combi$FamilySize)
combi$FamID <- paste(as.character(combi$FamilySize),combi$FamName,  combi$Embarked, combi$Pclass,  sep="-")

#combi$fseq <- unlist(sapply(unique(combi$FamlID), function(id) 1:table(combi$FamlID)[id]))
#combi$FamID <- paste(combi$fseq, combi$FamID, sep="-")
length(unique(combi$FamID))
# library(data.table)
# setDT(combi)[, fseq := seq_len(.N), .(FamID)]
# setDT(combi)[, fseq := match(FamName, unique(FamName)), by = .(FamilySize)]
# combi %>%
#   group_by(FamName) %>%
#   mutate(fseq = match(FamilySize, unique(FamilySize)))
# combi$fseq <- ave(combi$FamID, FUN=seq_along)
# combi$FamID <- paste(as.character(combi$fseq), combi$FamID)
#combi$FamID <- ifelse(combi$FamilySize <=2, 'small',combi$FamID )

fcheck <- count(combi[combi$FamilySize!=1,], c('FamID','SibSp','Parch'))
fcheck$fsbp <- fcheck$freq*fcheck$SibSp
fcheck$fpch<- fcheck$freq*fcheck$Parch
fcheck$sbspmsplcd <- ifelse(fcheck$fsbp%%2 != 0,1,0)
fcheck$pchmsplcd<- ifelse(fcheck$fpch%%2 != 0,1,0)

#edit(fcheck)
nrow(fcheck[fcheck$sbspmsplcd == 1,])
nrow(fcheck[fcheck$pchmsplcd == 1,])

combi$utSibSp<- NULL
combi$utParch <- NULL

combi$utSibSp <- fcheck$sbspmsplcd[match(combi$FamID, fcheck$FamID)]
combi$utParch <- fcheck$pchmsplcd[match(combi$FamID, fcheck$FamID)]

combi$utSibSp <- ifelse(is.na(combi$utSibSp)== TRUE, 0,combi$utSibSp)
combi$utParch <- ifelse(is.na(combi$utParch)== TRUE, 0,combi$utParch)

combi$utSibSp <- as.factor(combi$utSibSp)
combi$utParch <- as.factor(combi$utParch)

### Network effect
# c('infant','children','teen','young adult','adult','old')

#netwrkefft <-ddply(combi,~FamID,summarise,inft_e =length(ifelse(combi$agegrp =='infant',1,0)))
# Infant
inft_e <- data.frame(table(combi$FamID[combi$agegrp =='infant']))
combi$inft_e <- inft_e$Freq[match(combi$FamID, inft_e$Var1)]
combi$inft_e <- ifelse(is.na(combi$inft_e)== TRUE, 0,combi$inft_e)
combi$inft_e <- ifelse((combi$inft_e!=0 & combi$agegrp =='infant'),combi$inft_e-1,combi$inft_e)
# male children
chld_m_e <-
  data.frame(table(combi$FamID[combi$agegrp == 'children' &
                                 combi$Sex == 'male']))
combi$chld_m_e <- chld_m_e$Freq[match(combi$FamID, chld_m_e$Var1)]
combi$chld_m_e <-
  ifelse(is.na(combi$chld_m_e) == TRUE, 0,combi$chld_m_e)
combi$chld_m_e <-
  ifelse((
    combi$chld_m_e != 0 &
      combi$agegrp == 'children' &
      combi$Sex == 'male'
  ),combi$chld_m_e - 1,combi$chld_m_e
  )

#Female Children
chld_f_e <-
  data.frame(table(combi$FamID[combi$agegrp == 'children' &
                                 combi$Sex == 'female']))
combi$chld_f_e <- chld_f_e$Freq[match(combi$FamID, chld_f_e$Var1)]
combi$chld_f_e <-
  ifelse(is.na(combi$chld_f_e) == TRUE, 0,combi$chld_f_e)
combi$chld_f_e <-
  ifelse((
    combi$chld_f_e != 0 &
      combi$agegrp == 'children' &
      combi$Sex == 'female'
  ),combi$chld_f_e - 1,combi$chld_f_e
  )
#Teen Male
teen_m_e <-
  data.frame(table(combi$FamID[combi$agegrp == 'teen' &
                                 combi$Sex == 'male']))
combi$teen_m_e <- teen_m_e$Freq[match(combi$FamID, teen_m_e$Var1)]
combi$teen_m_e <-
  ifelse(is.na(combi$teen_m_e) == TRUE, 0,combi$teen_m_e)
combi$teen_m_e <-
  ifelse((
    combi$teen_m_e != 0 &
      combi$agegrp == 'teen' &
      combi$Sex == 'male'
  ),combi$teen_m_e - 1,combi$teen_m_e
  )
# Teen Female
ya_m_e <-
  data.frame(table(combi$FamID[combi$agegrp == 'teen' &
                                 combi$Sex == 'female']))
combi$ya_m_e <- ya_m_e$Freq[match(combi$FamID, ya_m_e$Var1)]
combi$ya_m_e <-
  ifelse(is.na(combi$ya_m_e) == TRUE, 0,combi$ya_m_e)
combi$ya_m_e <-
  ifelse((
    combi$ya_m_e != 0 &
      combi$agegrp == 'teen' &
      combi$Sex == 'female'
  ),combi$ya_m_e - 1,combi$ya_m_e
  )
# young adult male
ya_m_e <-
  data.frame(table(combi$FamID[combi$agegrp == 'young adult' &
                                 combi$Sex == 'male']))
combi$ya_m_e <- ya_m_e$Freq[match(combi$FamID, ya_m_e$Var1)]
combi$ya_m_e <-
  ifelse(is.na(combi$ya_m_e) == TRUE, 0,combi$ya_m_e)
combi$ya_m_e <-
  ifelse((
    combi$ya_m_e != 0 &
      combi$agegrp == 'young adult' &
      combi$Sex == 'male'
  ),combi$ya_m_e - 1,combi$ya_m_e
  )

# young adult female
ya_f_e <-
  data.frame(table(combi$FamID[combi$agegrp == 'young adult' &
                                 combi$Sex == 'female']))
combi$ya_f_e <- ya_f_e$Freq[match(combi$FamID, ya_f_e$Var1)]
combi$ya_f_e <-
  ifelse(is.na(combi$ya_f_e) == TRUE, 0,combi$ya_f_e)
combi$ya_f_e <-
  ifelse((
    combi$ya_f_e != 0 &
      combi$agegrp == 'young adult' &
      combi$Sex == 'male'
  ),combi$ya_f_e - 1,combi$ya_f_e
  )

# Adult Male
a_m_e <-
  data.frame(table(combi$FamID[combi$agegrp == 'adult' &
                                 combi$Sex == 'male']))
combi$a_m_e <- a_m_e$Freq[match(combi$FamID, a_m_e$Var1)]
combi$a_m_e <-
  ifelse(is.na(combi$a_m_e) == TRUE, 0,combi$a_m_e)
combi$a_m_e <-
  ifelse((
    combi$a_m_e != 0 &
      combi$agegrp == 'adult' &
      combi$Sex == 'male'
  ),combi$a_m_e - 1,combi$a_m_e
  )

# Adult Female
a_f_e <-
  data.frame(table(combi$FamID[combi$agegrp == 'adult' &
                                 combi$Sex == 'female']))
combi$a_f_e <- a_f_e$Freq[match(combi$FamID, a_f_e$Var1)]
combi$a_f_e <-
  ifelse(is.na(combi$a_f_e) == TRUE, 0,combi$a_f_e)
combi$a_f_e <-
  ifelse((
    combi$a_f_e != 0 &
      combi$agegrp == 'adult' &
      combi$Sex == 'female'
  ),combi$a_f_e - 1,combi$a_f_e
  )

# Old Male
o_m_e <-
  data.frame(table(combi$FamID[combi$agegrp == 'old' &
                                 combi$Sex == 'male']))
combi$o_m_e <- o_m_e$Freq[match(combi$FamID, o_m_e$Var1)]
combi$o_m_e <-
  ifelse(is.na(combi$o_m_e) == TRUE, 0,combi$o_m_e)
combi$o_m_e <-
  ifelse((
    combi$o_m_e != 0 &
      combi$agegrp == 'old' &
      combi$Sex == 'male'
  ),combi$o_m_e - 1,combi$o_m_e
  )

## Old FeMale
o_f_e <-
  data.frame(table(combi$FamID[combi$agegrp == 'old' &
                                 combi$Sex == 'female']))
combi$o_f_e <- o_f_e$Freq[match(combi$FamID, o_f_e$Var1)]
combi$o_f_e <-
  ifelse(is.na(combi$o_f_e) == TRUE, 0,combi$o_f_e)
combi$o_f_e <-
  ifelse((
    combi$o_f_e != 0 &
      combi$agegrp == 'old' &
      combi$Sex == 'female'
  ),combi$o_f_e - 1,combi$o_f_e
  )

# Create a new field called fam group

combi$famgrp <- combi$FamID
combi$famgrp <- ifelse(combi$FamilySize ==1, 'single', combi$famgrp)
combi$famgrp <- ifelse(combi$FamilySize > 1 & combi$FamilySize <=4, 'small',combi$famgrp)
combi$famgrp <- as.factor(combi$famgrp)

# Converting to fractions
# combi$inft_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$inft_e/(combi$SibSp+ combi$Parch),0)
# combi$chld_m_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$chld_m_e/(combi$SibSp+ combi$Parch),0)
# combi$chld_f_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$chld_f_e/(combi$SibSp+ combi$Parch),0)
# combi$teen_m_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$teen_m_e/(combi$SibSp+ combi$Parch),0)
# combi$teen_f_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$teen_f_e/(combi$SibSp+ combi$Parch),0)
# combi$ya_m_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$ya_m_e/(combi$SibSp+ combi$Parch),0)
# combi$ya_f_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$ya_f_e/(combi$SibSp+ combi$Parch),0)
# combi$a_m_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$a_m_e/(combi$SibSp+ combi$Parch),0)
# combi$a_f_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$a_f_e/(combi$SibSp+ combi$Parch),0)
# combi$o_m_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$o_m_e/(combi$SibSp+ combi$Parch),0)
# combi$o_f_e <- ifelse((combi$SibSp+ combi$Parch)!= 0,combi$o_f_e/(combi$SibSp+ combi$Parch),0)




# table(combi$FamID)
# schk <- count(combi[combi$SibSp!=0,], c('FamID', 'SibSp'))
# schk$frqsm <- schk$SibSp* schk$freq
# rschk <-ddply(schk,~FamID,summarise,rfrqsm=sum(frqsm, na.rm = TRUE))
# edit(rschk[rschk$rfrqsm%%2 !=0,])
# 
# pchk <- count(combi[combi$Parch!=0,], c('FamID', 'Parch'))
# pchk$frqsm <- pchk$freq*pchk$Parch
# rpchk <-ddply(pchk,~FamID,summarise,rfrqsm=sum(frqsm, na.rm = TRUE))
# edit(rpchk[rpchk$rfrqsm%%2 !=0,])


#frqchk1 <- freqchk[freqchk$FamilySize > freqchk$freq,]
#freqchk$m.famsz <- ifelse(freqchk$FamilySize > freqchk$freq, freqchk$freq, freqchk$FamilySize)

# famlist <- data.frame(table(combi$FamName))
# combi$Famnfreq <- famlist$Freq[match(combi$FamName, famlist$Var1)]
# combi$Famf <- paste(as.character(combi$Famnfreq), combi$FamName, sep="")
# combi$Famf <- ifelse(combi$Famnfreq <=3, 'small',combi$Famf )
# combi$Famf <- factor(combi$Famf)
# table(combi$Famf)

# ticket          Ticket Number
summary(combi$ticket)

library(stringi)
combi$TktPre <- NA
combi$TktNum <- NA
# Get the raw TktPre (or NA), TktNum
combi[,c('TktPre','TktNum')] <- stri_match(combi$Ticket, regex='(.* )?([0-9]+)' ) [,-1]
length(which(!complete.cases(combi$TktNum)))
combi$frstnumtkt <- as.factor(substr(as.character(combi$TktNum),1,1))
combi$frstnumtkt <- ifelse(as.integer(combi$frstnumtkt) > 3, '>3', combi$frstnumtkt)
combi$frstnumtkt <- ifelse(is.na(combi$frstnumtkt)== TRUE, 'NA', combi$frstnumtkt)
combi$frstnumtkt <-as.factor(combi$frstnumtkt)

library(stringr)
combi$TktPre <- str_replace_all(combi$TktPre, "[[:punct:]]", "")
combi$TktPre_1 <- substr(as.character(combi$TktPre),1,1)
combi$TktPre[is.na(combi$TktPre)]<- 'NA'
combi$TktPre_1[is.na(combi$TktPre_1)]<- 'NA'
combi$TktPre <- as.factor(combi$TktPre)
combi$TktPre_1 <- as.factor(combi$TktPre_1)


# fare            Passenger Fare
combi$Fare[is.na(combi$Fare)== TRUE]<- median(combi$Fare, na.rm=TRUE)
summary(combi$Fare)
# cabin           Cabin
combi$Cabin <- as.character(combi$Cabin)
combi$Cabin <- ifelse(is.na(combi$Cabin)== TRUE, 'NA',combi$Cabin)

#combi$Cabin[is.na(trimws(combi$Cabin))]<- 'NA'
#combi$Cabin <- as.factor(trimws(combi$Cabin))
combi$deck <- 'NA'
combi$deck <- substr(trimws(combi$Cabin),1,1)
combi$deck <- ifelse(is.na(combi$deck) == TRUE, 'NA', combi$deck)
combi$deck <- as.factor(combi$deck)
table(combi$deck)

#$avgfare <- combi$Fare/combi$FamilySize
#cafare <-ddply(combi,~substr(Cabin,1,1),summarise,mean=mean(avgfare, na.rm = TRUE),sd=sd(avgfare, na.rm = TRUE))

# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)
combi$Embarked[is.na(combi$Embarked)== TRUE] <- 'NA'
table(combi$Embarked, combi$Survived)

combi$Embarked <- as.factor(combi$Embarked)

combi1<- combi

combi.na <- names(combi)[sapply(combi, function(x) length(which(is.na(x))))!=0]
combi.fac <-names(combi)[sapply(combi, is.factor)]
combi.fac <- combi.fac[combi.fac!='Survived']
combi.incmplt <-
  names(combi)[sapply(combi, function(x)
    length(which(!complete.cases(x))))>0]
combi.faclvlle2<- names(combi[,combi.fac])[sapply(combi[,combi.fac], function(x)
  length(levels(x)))<2]
sapply(combi[,combi.fac], function(x) length(levels(x)))
combi.fac <- setdiff(combi.fac, combi.faclvlle2)
combi.fac <- setdiff(combi.fac, combi.incmplt)
combi.oth<- names(combi)[!sapply(combi, is.factor)]
combi.oth <- setdiff(combi.oth, combi.incmplt)
myvars <- c(combi.oth, combi.fac)



edit(myvars)



#mdvars <- c("Sex", "Age", "Fare", "inft_e", "chld_m_e", "chld_f_e", "teen_m_e", "ya_m_e", "ya_f_e", "a_m_e", 
#            "a_f_e", "o_m_e", "o_f_e", "Pclass", "FamID","Embarked", "Title","SibSp.flg", "Parch.flg", "utSibSp", "utParch", "frstnumtkt", 
#           "TktPre_1", "deck")
mdvars <-
  c(
    "SibSp", "Parch", "Fare", "ya_f_e", "a_f_e", "o_m_e", "o_f_e", "Pclass", "Sex", "Embarked", "Title", "SibSp.flg", "Parch.flg", "utSibSp", "utParch", "famgrp", "frstnumtkt", "TktPre_1", "deck"
  )

ttr <- combi[combi$flg == 'ttr',]
y <- as.factor(ttr$Survived)

tte <- combi[combi$flg == 'tte',]
tte$Survived <- NULL

train.frmla <- as.formula(paste("as.factor(y)~ ", paste(mdvars, collapse= "+")))
#ttr <- ttr[mdvars]
#te.mdvars <- c('PassengerId', mdvars)
#te.mdvars
#tte <- tte[te.mdvars]

library(randomForest)

set.seed(415)
fit <- randomForest(train.frmla,
                    data=ttr, 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, tte)
train.pred <- predict(fit, ttr)
submit <- data.frame(PassengerId= row.names(tte), Survived = Prediction)
write.csv(submit, file = "secondforest.csv", row.names = FALSE)

library(glmnet)

Titanic.lm.1 <- glm(train.frmla, data = ttr, family=binomial("logit"))

library(caret)

rf.grid <- data.frame(.mtry = c(2, 3))
set.seed(35)
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)
rf.tune <- train(train.frmla, 
                 data = ttr,
                 method = "rf",
                 metric = "ROC",
                 tuneGrid = rf.grid,
                 trControl = cv.ctrl)

