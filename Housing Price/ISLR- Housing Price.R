rm(sales, tsales, sales1, train, test)
dir <- 'D:\\MyStuff\\Housing Price'
setwd(dir)
sales <- read.csv('kc_house_data.csv', stringsAsFactors = FALSE)
#Back up data set
tsales <- read.csv('kc_house_data.csv', stringsAsFactors = FALSE)

str(sales)
attach(sales)
summary(price)

# basic manipulation
sales$saleyrmnth <- as.integer(format(substr(sales$date,1,6), format ="Y%-m%"))
bedrooms <- as.factor(bedrooms)
bathrooms<- as.factor(bathrooms)
floors <- as.factor(floors)
waterfront<-as.factor(waterfront)
view <- as.factor(view)
condition<- as.factor(condition)
grade<- as.factor(grade)
zipcode <- as.factor(zipcode)

sales1 <-
  subset(sales, select = -c(id, date))


pairs(sales1)


