#Q1 CART
ebay.df <- read.csv("BH2data.csv")
#a
summary(ebay.df)
#b
hist(ebay.df$CAT..MEDV)
#c
set.seed(1234)
train.rows <- sample(rownames(ebay.df), dim(ebay.df)[1]*0.6)
valid.rows <- sample(setdiff(rownames(ebay.df), train.rows), dim(ebay.df)[1]*0.4)
train.data <- ebay.df[train.rows, ]
valid.data <- ebay.df[valid.rows, ]
#using best-pruned tree
#install.packages(c("rpart", "rpart.plot"))
library(rpart); library(rpart.plot)
library(forecast); library(caret); library(e1071)
library(randomForest);library(adabag)
#d
set.seed(1234)
cv.ct <- rpart(ebay.df$CAT..MEDV ~ ., data = ebay.df, method = "class", control= rpart.control(minsplit = 2,minbucket = 1,cp = 0))
prp(cv.ct,type = 1, extra = 1, split.font = 1, varlen = -10)
#e
cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"] #this will take the cp value with minimum xerror
pruned.ct <- prune(cv.ct,cp=cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
printcp(cv.ct)  # Print out the cp table of cross-validation errors.
prp(pruned.ct,type = 1, extra = 1, split.font = 1, varlen = -10)
#f
ebay.pred.valid <- predict(pruned.ct,valid.data,type = "class")
confusionMatrix(ebay.pred.valid,as.factor(valid.data$CAT..MEDV)) #since 0.5 is the default cutoff value taken
#g
newdata <- read.csv("newBH2data1.csv")

predicted_class_score_BH2 <- predict(pruned.ct, newdata, type = "class")
predicted_class_score_BH2 # To get the predicted class

predicted_class_prob_BH2 <- predict(pruned.ct, newdata, type = "prob")
predicted_class_prob_BH2 # To get the predicted probability

#2 K means Clustering
#a
utilities.df <- read.csv("Utilities_Question14.csv")
row.names(utilities.df) <- utilities.df[,1]
utilities.df <- utilities.df[,-1]

# normalized distance:
#utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df) 

# run kmeans algorithm 
#b
set.seed(2)
km <- kmeans(utilities.df.norm, 4)

# "4" Specifies the desired number of clusters =64

# show cluster membership
#c
km$cluster

# Cluster Size
#d
km$size
