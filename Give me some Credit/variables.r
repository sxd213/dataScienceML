varlist <-names(train)
dim(train)

length(varlist)

select.satus <- rep('o',length(varlist))

var.df <- cbind(varlist,select.satus)
edit(var.df)
write.csv(var.df, file = "vardef.csv")
var.df <- read.csv("vardef.csv",  header = TRUE, sep = ",", row.names = 1, stringsAsFactors = FALSE)

train[var.df$varlist[var.df$select.satus=='x']]

