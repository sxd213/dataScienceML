library(ggplot2)

rm(sales, sales1, train, test)

setwd('D:\\MyStuff\\Housing Price')
sales <- read.csv('kc_house_data.csv', stringsAsFactors = FALSE)
tsales <- read.csv('kc_house_data.csv', stringsAsFactors = FALSE)
cl <- colnames(sales)
length(sales)
str(sales)

library(gtools)
sales$pricdec <-quantcut( sales$price, seq(0,1,by=0.1) )
aggregate(
  price ~ pricdec, data = sales, FUN = function(x) {
    length(x)
  }
)


#date"      
sales$saleyrmnth <- format(substr(sales$date,1,6), format ="Y%-m%")
unique(sales$saleyrmnth)

sales$mnth <- format(substr(sales$date,5,6), format ="m%")
unique(sales$mnth)

aggregate(
  price ~ saleyrmnth, data = sales, FUN = function(x) {
    mean(x)
  }
)

aggregate(
  price ~ mnth, data = sales, FUN = function(x) {
    mean(x)
  }
)

sales$saleyrmnth<- as.factor(sales$saleyrmnth)
sales$mnth <- as.integer(sales$mnth)

ggplot(sales, aes(sales$saleyrmnth, fill=sales$pricdec)) + geom_bar()

ggplot(sales, aes(as.factor(saleyrmnth),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Sqft lot in Seattle") +
  xlab("Month") + ylab("Price")

# Boxplot for yrmnth
ggplot(sales, aes(as.factor(mnth),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Sqft lot in Seattle") +
  xlab("Month") + ylab("Price")

## Add sunshine hours as information and rank order months by sunshine
# There is a trend that the winter and rainy months have throughs in sales, 
#summer and spring sees upswings. Hence in order to get additional information mapped
#hours sunlight to the months. Using Seattle as a surrogate for the whole
#http://www.usclimatedata.com/climate/seattle/washington/united-states/uswa0395
x <-sort(unique(sales$mnth))
y <- c(74,99,154,201,247,234,304,248,197,122,77,62)
df <- data.frame(x,y)
names(df) <- c("month","sunshine")


df$ssr <- rank(df$sunshine)

for (i in 1:nrow(df)){
  sales$ssr[sales$mnth == df[i,1]] <- df[i,3]
}

qplot(
  as.factor(sales$ssr), data = sales, geom = "bar", fill = sales$pricdec, position =
    "fill"
)

sales$ssr <- as.factor(sales$ssr)


ggplot(sales, aes(as.factor(ssr),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Sqft lot in Seattle") +
  xlab("Month rank in sunshine") + ylab("Price")


#"bedrooms" 
aggregate(
  price ~ bedrooms, data = sales, FUN = function(x) {
    mean(x)
  }
)



head(sales[sales$bedrooms == 2,])

ggplot(sales, aes(as.factor(bedrooms^2),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Bedrooms") + ylab("Price")

#The differentiation is now lot clearer

sales$sqbedrooms<- as.numeric(sales$bedrooms)^2
sales$bedrooms<- as.numeric(sales$bedrooms)


#[5] "bathrooms"   

ggplot(sales, aes(as.factor(bathrooms),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Bathrooms") + ylab("Price")

sales$bathrooms <- as.numeric(sales$bathrooms)

unique(sales$bathrooms)



summary(sales$bathrooms)
#bedroom&bathroom interactions

sales$bb <- as.numeric(sales$bathrooms)*as.numeric(sales$bedrooms)

ggplot(sales, aes(as.factor(bb),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Bed&Bath") + ylab("Price")

qplot(sales$bb,sales$price,colour=sales$price,
      xlab ="Square feet", ylab="price($)")+
  geom_smooth(method = "lm")

#"sqft_living"   

qplot(sales$sqft_living,sales$price,colour= sales$price,
      xlab ="Square feet", ylab="price($)")+
  geom_smooth(method = "lm")

sales$sqft_living <- as.numeric(sales$sqft_living)

#"sqft_lot" 
qplot(sales$sqft_lot,sales$price,colour= sales$price,
      xlab ="Square feet", ylab="price($)")+
  geom_smooth(method = "lm")

sales$sqft_lot <- as.numeric(sales$sqft_lot)


##"floors"
ggplot(sales, aes(as.factor(floors),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Floors") + ylab("Price")

sales$floors <- as.factor(sales$floors)

#[9] "waterfront"   
ggplot(sales, aes(as.factor(waterfront),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Waterfront") + ylab("Price")



sales$waterfront <- as.factor(sales$waterfront)


#"view"          
ggplot(sales, aes(as.factor(view),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("View") + ylab("Price")

sales$view <- as.factor(sales$view)

#"condition"     
ggplot(sales, aes(as.factor(condition),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Condition") + ylab("Price")

sales$condition <- as.factor(sales$condition)

#"grade" 
ggplot(sales, aes(as.factor(grade),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Bedrooms in Seattle") +
  xlab("Grade")+ ylab("Price")

qplot(sales$grade,sales$price,colour= sales$price,
      xlab ="Square feet", ylab="price($)")+
  geom_smooth(method = "lm")

sales$grade <- as.factor(sales$grade)


#[13] "sqft_above"    "sqft_basement" 

nrow(sales[sales$sqft_living == sales$sqft_above+sales$sqft_basement,])

qplot(sales$sqft_living ,sales$price,colour= sales$price,
      xlab ="Square feet", ylab="price($)")+
  geom_smooth(method = "lm", se = TRUE)
      
sales$sqft_above <- as.numeric(sales$sqft_above)
sales$sqft_basement <- as.numeric(sales$sqft_basement)

#"yr_built"       
class(sales$yr_built)
sales$age <-as.numeric(substr(sales$date,1,4))- sales$yr_built
head(sales[sales$age == 109,])

qplot(sales$age ,sales$price,colour= sales$price,
      xlab ="Age", ylab="price($)")+
  geom_smooth(method = "lm", se = TRUE)

#"yr_renovated"
#df[df$Name == "John_Smith" & df$State == "WI", "Name"] <- "John_Smith1"
sales[sales$yr_renovated==0,"yr_renovated"] <- sales$yr_built[sales$yr_renovated==0]
sales$reage <-as.integer(substr(sales$date,1,4))- sales$yr_renovated
qplot(sales$reage ,sales$price,colour= sales$price,
      xlab ="Age", ylab="price($)")

sales$reage <- as.numeric(sales$reage)

#[17] "zipcode"      
ggplot(sales, aes(as.factor(zipcode),price)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Zipcode in Seattle") +
  xlab("Zipcode")+ ylab("Price")

sales$zipcode <- as.factor(sales$zipcode)

qplot(sales$zipcode ,sales$price,colour= sales$pricdec,
      xlab ="Zipcode", ylab="price($)")


#"lat"           "long" 
# library(sp)
# 
# # Space Needle 47.6205° N, -122.3493° E
# # Test :- 47.6300 , -122.3500
# 
summary(sales$lat)
# 
# lon2 <- -122.3480
# lat2 <- 47.6210
spndl <- c(47.6205, -122.3493)
spndl[1]

haversine<- function(long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  long1<- -122.3493
  lat1 <- 47.6205
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

sales$spnedist <- mapply(haversine, sales$long, sales$lat)

sales[sales$spnedist== 5599,]

summary(log(sales$spnedist))


qplot(log(sales$spnedist) ,sales$price,colour= sales$price,
      xlab ="Distance from Space Needle", ylab="price($)")+
  geom_smooth(method = "lm", se = TRUE)
  

qplot(log(spnedist), data=sales, geom="histogram", bins = 50, fill= as.factor(sales$pricdec))


summary(sales$lat)
summary(sales$long)



#"sqft_living15"
qplot(sales$sqft_living ,sales$sqft_living15,colour= sales$price,
      xlab ="Square feet", ylab="sqft living15")+
  geom_smooth(method = "lm", se = TRUE)
#[21] "sqft_lot15"
qplot(sales$sqft_lot ,sales$sqft_lot15,colour= sales$price,
      xlab ="Sqft lot", ylab="sqft lot15")+
  geom_smooth(method = "lm", se = TRUE)

sales$sqft_living15 <- as.numeric(sales$sqft_living15)
sales$sqft_lot15 <- as.numeric(sales$sqft_lot15)

# price
library(ggplot2)
qplot(sales$sqft_living,sales$price,colour=sales$price,
      xlab ="Squre feet", ylab="price($)")

sales$dpsqft <- sales$price/ (sales$sqft_living+sales$sqft_lot)

ggplot(sales, aes(as.factor(zipcode),dpsqft)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Zipcode in Seattle") +
  xlab("Zipcode")+ ylab("psqft price")

ggplot(sales, aes(as.factor(bb),dpsqft)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Zipcode in Seattle") +
  xlab("BB interaction")+ ylab("psqft price")

aggregate(
  dpsqft ~ bb+spnedist, data = sales, FUN = function(x) {
    mean(x)
  }
)

sales$dpsqft15 <- sales$price/ (sales$sqft_living15+sales$sqft_lot15)

ggplot(sales, aes(as.factor(zipcode),dpsqft15)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Zipcode in Seattle") +
  xlab("Zipcode")+ ylab("psqft price")

ggplot(sales, aes(as.factor(bb),dpsqft15)) +
  geom_boxplot(color="blue", fill="green") +
  ggtitle("Box Plot for Zipcode in Seattle") +
  xlab("BB interaction")+ ylab("psqft price")


sales$saleyrmnth <- as.integer(sales$saleyrmnth)
sales$mnth <- as.integer((sales$mnth))
sales$lspnedist <- log(sales$spnedist)
#sales$lbb <- log(sales$bb)

ncol(sales)
str(tsales)

summary(sales$waterfront)
#Preparing data for initial model
# Taking out some variables that were created for initial exploration
#
str(sales)
sales1 <-
  subset(
    sales, select = -c(
      id, date, dpsqft, bedrooms, bathrooms, dpsqft15,pricdec,lat,long, sqft_above,sqft_basement, mnth, yr_built, yr_renovated, spnedist
    )
  )

str(sales1)

