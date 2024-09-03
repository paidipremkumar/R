
#Problem 2.11
set.seed(1234)
housing.df <- read.csv("ToyotaCorolla.csv",header = TRUE)
xtotal <- model.matrix(~ 0 + Fuel_Type + Color, data = housing.df)

xtotal <- as.data.frame(xtotal) # convert matrix to data frame
t(t(names(xtotal))) # check the names of the dummy variables
xtotal <- xtotal[,-3] # removing the dummy variable FuelPetrol
head(xtotal)
housing.df <- cbind(housing.df[,-c(8,11)],xtotal) #Joing the data with the dummy variables instead of Fuel_Type and Color Columns
t(t(names(housing.df)))
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.5) 
# randomly sample 30% of the row IDs for validation (setdiff used to draw records not in training set)
valid.rows <- sample(setdiff(rownames(housing.df), train.rows), dim(housing.df)[1]*0.3)

test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))

train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]

#Problem 2 Q1(a)

library(ggplot2)
# Basic scatter plot
dataframe <- read.csv("car_insurance.csv")
set.seed(1234)
s <- sample(row.names(dataframe),24)
dataframe[s,]
dim(dataframe)
ggplot(dataframe, aes(y=Total_Claim_Amount, x=Income, shape=Response, color=Response)) +geom_point()+ geom_smooth(method=lm, se=FALSE, fullrange=FALSE)


#Problem 2 Q2

library(ggplot2)
library(dplyr)
set.seed(1234)
df<- read.csv("LaptopSalesJanuary2008.csv")
df$Store.Postcode <- as.factor(df$Store.Postcode) 
df %>% group_by(Store.Postcode) %>% summarise(Average.Retail.Price = mean(Retail.Price)) %>% mutate(order = Store.Postcode) %>% ggplot(aes(order,Average.Retail.Price))+geom_col()
df %>% subset(Store.Postcode==c("N17 6QA", "W4 3PH")) %>% group_by(Store.Postcode) %>% ggplot(aes(y=Retail.Price,color=Store.Postcode))+geom_boxplot()

#Problem 3

set.seed(1234)
dataframe <- read.csv("Movies2016.csv",stringsAsFactors=TRUE,header = TRUE)
library(ggplot2)
dataframe$Number.of.Theaters <- as.numeric(dataframe$Number.of.Theaters) 
h<-hist(dataframe$Opening.Gross.Sales....millions.,main="Histogram of Opening Gross Sales",xlab="Opening Gross Sales in Millions",ylab="Frequency")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
h1<-hist(dataframe$Total.Gross.Sales....millions.,main="Histogram of Total Gross Sales",xlab="Total Gross Sales in Millions",ylab="Frequency")
text(h1$mids,h1$counts,labels=h1$counts, adj=c(0.5, -0.5))
h2<-hist(dataframe$Number.of.Theaters,main="Histogram of Number of Theaters",xlab="Number of theaters",ylab="Frequency")
text(h2$mids,h2$counts,labels=h2$counts, adj=c(0.5, -0.5))
h3<-hist(dataframe$Weeks.in.Release,main="Histogram of Weeks in release",xlab="Weeks in release",ylab="Frequency")
text(h3$mids,h3$counts,labels=h3$counts, adj=c(0.5, -0.5))
ggplot(dataframe, aes(x=Opening.Gross.Sales....millions., y=Total.Gross.Sales....millions.)) + geom_point()
ggplot(dataframe, aes(x=Number.of.Theaters, y=Total.Gross.Sales....millions.)) + geom_point()
ggplot(dataframe, aes(x=log(Weeks.in.Release), y=log(Total.Gross.Sales....millions.))) + geom_point() 
