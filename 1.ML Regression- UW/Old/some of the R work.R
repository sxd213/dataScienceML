library(ggplot2)
setwd('D:\\MyStuff\\Cousera\\ML Regression- UW')

sales <- read.csv('kc_house_data.csv', stringsAsFactors = FALSE)
train <- read.csv('kc_house_train_data.csv', stringsAsFactors = FALSE)
test <- read.csv('kc_house_test_data.csv', stringsAsFactors = FALSE)

analysis <- sales

names(analysis)
nrow(analysis)
length(complete.cases(analysis))
summary(analysis)
attach(analysis)

head(analysis)

#[1] "id"            "date"           

analysis$saledate <- strptime(analysis$date, "%Y-%m-%d")
analysis$yrmnth <- format(substr(analysis$date,1,6), format ="Y%-m%")

qplot(yrmnth, data=analysis, geom="bar")
qplot(price, data=analysis, geom="histogram", bins = 250)

#"price"         
qplot(price, data=analysis, geom="histogram", bins = 25)

summary(price)


#"bedrooms"      "bathrooms"  

bbi$sales <- sales$bathrooms*sales$bedrooms

aggregate(
  sales$price ~sales$bbi, data = train, FUN = function(x) {
    mean(x)
  }
)



table(bedrooms)

table(bathrooms)
#[6] "sqft_living"   "sqft_lot"      "floors"        "waterfront"    "view"         
#[11] "condition"     "grade"         "sqft_above"    "sqft_basement" "yr_built"     
#[16] "yr_renovated"  "zipcode"       "lat"           "long"          "sqft_living15"
#[21] "sqft_lot15"    "saledate"      "yrmnth"



qplot()




# Date.form1 <- as.Date(as.character(sales$date), "%Y-%m")


length(sales$saledate)
