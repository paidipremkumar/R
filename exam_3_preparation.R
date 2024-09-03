library(rpart);
library(rpart.plot);
library(forecast); 
library(caret);
library(rpart.plot);
library(ggplot2);
library(neuralnet);
library(nnet);
library(e1071);
library(arules);
library(arulesViz);

#question-1

#a
df <- read.csv("UniversalBank Short.csv")
Loan <- df$Personal.Loan # Create a column with names
df$Personal.Loan <- as.factor(df$Personal.Loan)

# Create a data frame without outcome
df1 <- df[,-5] # Remove the Personal Loan Column
# Scale the data for Neural Network ---------------

max = apply(df1,2,max)
max
min = apply(df1,2,min)
min
scalednumdata.df <- as.data.frame(scale(df1, center = min, scale = max-min)) 
View(scalednumdata.df)


# Build back the data frame with the target variable
univ_scaled.df <- cbind(scalednumdata.df, Loan)

#-----end of a--------
#b
set.seed(1234)

train.index <- sample(c(1:dim(univ_scaled.df)[1]), dim(univ_scaled.df)[1]*0.6)  
train.df <- univ_scaled.df[train.index, ]
valid.df <- univ_scaled.df[-train.index, ]

#----end of b ---------

#c

nnloan2 <- neuralnet(Loan ~., data=train.df, act.fct = "logistic", linear.output = FALSE, hidden = 2)

plot(nnloan2, rep = "best")

#-----end of c------

#d
nnloan2$result.matrix


validation.prediction <- compute(nnloan2,valid.df) 

validation.class <- apply(validation.prediction$net.result,1,which.max)-1

confusionMatrix(as.factor(validation.class), as.factor(valid.df$Loan))
#------end of question-1----------

#question-2

#a

df3=read.csv("Coursetopics.csv")
df3.mat <- as.matrix(df3)
course.trans <- as(df3.mat, "transactions") 
inspect(course.trans)
rules <- apriori(course.trans)
inspect(rules)

#b
rules1 <- apriori(course.trans, parameter = list(supp = 0.003, conf = 0.8, target = "rules"))
inspect(rules1)
inspect(head(sort(rules1, by = "lift"), n = 10))
