# A Problem 11.3 using NNW Method
library(measures)
library(neuralnet)
car.df <- read.csv("ToyotaCorolla.csv")
set.seed(1234) # Compare resulting nn$weights with and without seed. 
xtotal <- model.matrix(~ 0 + Fuel_Type, data = car.df)

xtotal <- as.data.frame(xtotal) # convert matrix to data frame
t(t(names(xtotal))) # check the names of the dummy variables
xtotal <- xtotal[,-3] 
car.df <- cbind(car.df[,-c(8,11)],xtotal)
car.df<-car.df[,c("Price","Age_08_04","KM","HP",'Automatic','Doors',"Quarterly_Tax","Mfr_Guarantee",
                  "Guarantee_Period","Airco","Automatic_airco","CD_Player","Powered_Windows",
                  "Sport_Model","Tow_Bar","Fuel_TypeCNG","Fuel_TypeDiesel")]
norm.values <-preProcess(car.df,method="range")
car.norm.df <-predict(norm.values, car.df)
# partition
train.index <- sample(c(1:dim(car.df)[1]), dim(car.df)[1]*0.6)  
train.data <- car.df[train.index, ]
valid.data <- car.df[-train.index, ]
nn <- neuralnet(Price~., data = train.data, linear.output = F, hidden = 5) #for 1 hidden layer with 5 nodes
nn <- neuralnet(Price~., data = train.data, linear.output = F, hidden =c(5,5)) #for 2 hidden layers with 5 nodes
nn$weights
prediction(nn)
plot(nn, rep="best")
library(caret)
predict.train <- compute(nn,train.data)$net.result
RMSE(train.data[,"Price"],predict.train)
predict <- compute(nn,valid.data)$net.result
RMSE(valid.data[,"Price"], predict)

# B a) i) Using CART Method for 9.3
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)
car.df <- read.csv("ToyotaCorolla.csv")
car.df
set.seed(1234)
train.index <- sample(c(1:dim(car.df)[1]), dim(car.df)
                      [1]*0.6)
train.df <- car.df[train.index, ]
valid.df <- car.df[-train.index, ]
tr <- rpart(Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax 
            + Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco +
              CD_Player +
              Powered_Windows + Sport_Model + Tow_Bar,
            data = train.df,
            method = "anova", minbucket = 1, maxdepth = 30, cp = 0.001)
pruned.ct <- prune(tr,cp=tr$cptable[which.min(tr$cptable[,"xerror"]),"CP"])
prp(tr)
rf <- randomForest(train.df$Price ~ train.df$Age_08_04 + train.df$KM + train.df$Fuel_Type + train.df$HP + train.df$Automatic + train.df$Doors + train.df$Quarterly_Tax 
                   + train.df$Mfr_Guarantee + train.df$Guarantee_Period + train.df$Airco + train.df$Automatic_airco +
                     train.df$CD_Player +
                     train.df$Powered_Windows + train.df$Sport_Model + train.df$Tow_Bar,data=train.df,ntree=1972,mtry=4,nodesize =5, importance=TRUE)

#ii)
varImpPlot(rf,type =1)

tr$variable.importance
# iii)
train.err <-predict(tr, train.df) -train.df$Price
valid.err <-predict(tr, valid.df) -valid.df$Price
err <-data.frame(Error =c(train.err, valid.err),Set =c(rep("Training", length(train.err)),rep("Validation", length(valid.err))))
boxplot(Error~Set, data=err, main="RMS Errors",xlab = "Set", ylab = "Error")
library(forecast)
car.lm.pred <- predict(pruned.ct,valid.df)
all.residuals <- valid.df$Price - car.lm.pred
accuracy(car.lm.pred,valid.df$Price)

#Using Linear Regression
car.df <- read.csv("ToyotaCorolla.csv")
set.seed(1234)
train.index <- sample(c(1:dim(car.df)[1]), dim(car.df)
                      [1]*0.6)
train.data <- car.df[train.index, ]
valid.data <- car.df[-train.index, ]
car.lm <- lm(train.df$Price ~ train.df$Age_08_04 + train.df$KM + train.df$Fuel_Type + train.df$HP + train.df$Automatic + train.df$Doors + train.df$Quarterly_Tax 
             + train.df$Mfr_Guarantee + train.df$Guarantee_Period + train.df$Airco + train.df$Automatic_airco +
               train.df$CD_Player +
               train.df$Powered_Windows + train.df$Sport_Model + train.df$Tow_Bar,data=train.data)
options(scipen = 999)
summary(car.lm)
library(forecast)
car.lm.pred <- predict(car.lm,valid.data)
all.residuals <- valid.data$Price - car.lm.pred
accuracy(car.lm.pred,valid.data$Price)
