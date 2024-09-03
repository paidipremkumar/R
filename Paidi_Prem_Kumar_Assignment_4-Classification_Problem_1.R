#Using Cart Method
ebay.df <- read.csv("eBayAuctions.csv")
set.seed(1234)
ebay.df$Duration <- as.factor(ebay.df$Duration)
train.rows <- sample(rownames(ebay.df), dim(ebay.df)[1]*0.6)
valid.rows <- sample(setdiff(rownames(ebay.df), train.rows), dim(ebay.df)[1]*0.4)
train.data <- ebay.df[train.rows, ]
valid.data <- ebay.df[valid.rows, ]
#using best-pruned tree
#install.packages(c("rpart", "rpart.plot"))
library(rpart); library(rpart.plot)
library(forecast); library(caret); library(e1071)
library(randomForest);library(adabag)


cv.ct <- rpart(Competitive. ~ ., data = train.data, method = "class", control= rpart.control(maxdepth = 7,minbucket = 50))
pruned.ct <- prune(cv.ct,cp=cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
printcp(cv.ct)  # Print out the cp table of cross-validation errors.
prp(pruned.ct,type = 1, extra = 1, split.font = 1, varlen = -10)

rf <- randomForest(as.factor(Competitive.)~.,data=train.data,ntree=1972,mtry=4,nodesize =5, importance=TRUE)
varImpPlot(rf,type =1)

ebay.pred.valid <- predict(pruned.ct,valid.data,type = "class")
confusionMatrix(ebay.pred.valid,as.factor(valid.data$Competitive.))

#Using NNW Method
install.packages("neuralnet")
install.packages("nnet")
install.packages("caret")
install.packages("e1071")
library(neuralnet); library(nnet); library(caret); library(e1071)
ebay.df <- read.csv("eBayAuctions.csv")

ebay.df$Duration <- as.factor(ebay.df$Duration)
train.rows <- sample(rownames(ebay.df), dim(ebay.df)[1]*0.6)
valid.rows <- sample(setdiff(rownames(ebay.df), train.rows), dim(ebay.df)[1]*0.4)
train.data <- ebay.df[train.rows, ]
valid.data <- ebay.df[valid.rows, ]
set.seed(1234) # Compare resulting nn$weights with and without seed. 
nn <- neuralnet(as.factor(Competitive.) ~ sellerRating+ClosePrice+OpenPrice, data = train.data, linear.output = F, hidden = 3)
nn$weights
prediction(nn)
plot(nn, rep="best")
library(caret)
predict <- compute(nn,valid.data)
predicted.class = apply(predict$net.result, 1, which.max) - 1
confusionMatrix(as.factor(predicted.class),as.factor(valid.data$Competitive.))


#Using Logistic Regression

ebay.df <- read.csv("eBayAuctions.csv")
set.seed(1234)
ebay.df$Duration <- as.factor(ebay.df$Duration)
train.rows <- sample(rownames(ebay.df), dim(ebay.df)[1]*0.6)
valid.rows <- sample(setdiff(rownames(ebay.df), train.rows), dim(ebay.df)[1]*0.4)
train.data <- ebay.df[train.rows, ]
valid.data <- ebay.df[valid.rows, ]
ebay.lm <- glm(Competitive. ~ ., data = train.data)
summary(ebay.lm)
ebay.lm.pred <- predict(ebay.lm, valid.data)
confusionMatrix(as.factor(ifelse(ebay.lm.pred>0.6,'1','0')), 
                as.factor(valid.data$Competitive.))
