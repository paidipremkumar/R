exam.df <- read.csv("EX1-Dataset-1.csv")
#1a
exam.df$CAT <- as.factor(exam.df$CAT)
str(exam.df$CAT)
#1b
summary(exam.df)
#1c
cor(exam.df$Target,exam.df[,c('Var1','Var2','Var3')])


#2a
boxplot(exam.df$Target,data=exam.df,main="Box plot for variable Target")

#2b

library(ggplot2)
ggplot(exam.df, aes(x = Var1, y = Target)) + geom_point()
ggplot(exam.df, aes(x = Var2, y = Target)) + geom_point()
ggplot(exam.df, aes(x = Var3, y = Target)) + geom_point()

#2c From the above scatter plots all the three variables VAr1,Var2,Var3 have strong positive relationship with the Target Variable i.e., positive linear relationship 


#3a
set.seed(1234)
#3b
train.rows <- sample(rownames(exam.df), dim(exam.df)[1]*0.6)
valid.rows <- setdiff(rownames(exam.df), train.rows)
train.data <- exam.df[train.rows,]
valid.data <- exam.df[valid.rows,]
#3c
mean(train.data$Target)
mean(valid.data$Target)

#4A
#a
model <-lm(Target ~ .,data=train.data)
summary(model)
#b
#Var1,Var2,Var3,Var4,VAr7,Var8,Var10 have high t value which means they are significant predictors. Since the adjusted R2 is 82.05% we can say that model appears to be useful

#4B
library(forecast)
library(rcompanion)
pred <- predict(model,newdata = valid.data,type = "response")

library(pROC)
r <- roc(exam.df$Target, pred)
plot.roc(r)
auc(r)

#5
new <- read.csv("Unknown.csv")
pred <- predict(model,newdata = new,type = "response")
pred

library(plotrix)
std.error(valid.data$Target)
