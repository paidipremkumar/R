#problem 5.7
data.df <- matrix(c(.03,0,.52,0,.38,0,.82,1,.33,0,.42,0,.55,1,.59,0,.09,0,.21,0,.43,0,.04,0,.08,0,.13,0,.01,0,.79,1,.42,0,.29,0,.08,0,.02,0), ncol=2, byrow=TRUE)
colnames(data.df)<-c("Propensity of 1","Actual")
data.df<- as.data.frame(data.df)
data.df
install.packages("caret")
library(caret)
library(e1071)

##cutoff = 0.25
confusionMatrix(as.factor(ifelse(data.df$`Propensity of 1`>0.25, '1', '0')), 
                as.factor(data.df$Actual))
#cutoff= 0.5
confusionMatrix(as.factor(ifelse(data.df$`Propensity of 1`>0.5, '1', '0')), 
                as.factor(data.df$Actual))
#cutoff = 0.75
confusionMatrix(as.factor(ifelse(data.df$`Propensity of 1`>0.75, '1', '0')), 
                as.factor(data.df$Actual))

install.packages("gains")
library(gains)
#creating decile lift chart
gain <- gains(data.df$Actual, data.df$`Propensity of 1`)
barplot(gain$mean.resp / mean(data.df$Actual), names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")


#PROBLEM - 6.1

library(readxl)
library(dplyr)
boston.df <- read.csv("BostonHousing.csv")
b.boston <- boston.df[,-14] #we don't need CAT.MEDV
reduced.boston <- b.boston %>% dplyr::select(c("MEDV","CRIM","CHAS","RM"))
b.lm <- lm(MEDV~.,data = b.boston)
lm <- lm(MEDV ~ ., data = reduced.boston)
summary(lm)

c.lm.pred <- predict(lm,data.frame("CRIM" = 0.1,"RM" = 6, "CHAS" = 0))
c.lm.pred
plot(boston.df)
boston.df[,-c(13,14)] %>% cor() %>% heatmap(,Rowv = NA,Colv = NA)
cor(boston.df,boston.df$MEDV)

n <- nrow(b.boston)
b.shuffle.idx <- sample(c(1:n),round(0.6*n))
b.train.df <- b.boston[b.shuffle.idx,]
b.valid.df <- b.boston[-b.shuffle.idx,]
lm.back <- step(b.lm,direction = "backward")
summary(lm.back)
library(forecast)
lm.back.pred <- predict(lm.back, b.valid.df)
accuracy(lm.back.pred, b.valid.df$MEDV)
lm.null <- lm(MEDV~1,data = boston.df)
lm.forward <- step(lm.null, 
                   scope = list(lower=lm.null, upper= b.lm),
                   direction = "forward")
summary(lm.forward)
lm.forward.pred <- predict(lm.forward, b.valid.df)
accuracy(lm.forward.pred, b.valid.df$MEDV)
lm.step <- step(b.lm, direction = "both")
summary(lm.step)
lm.step.pred <- predict(lm.step,b.valid.df)
accuracy(lm.step.pred,b.valid.df$MEDV)
comparetest <- data.frame( 
  backwards = c(accuracy(lm.back.pred,b.valid.df$MEDV)),
  forward =c(accuracy(lm.forward.pred, b.valid.df$MEDV)),
  step = c(accuracy(lm.step.pred,b.valid.df$MEDV))
)
rownames(comparetest) <- c("ME","RMSE","MAE","MPE","MAPE")
comparetest
library(gains)
actual = b.valid.df$MEDV
#lift for backwards
gain1 = gains(actual, 
              lm.back.pred,
              group = 10)

plot(c(0, gain1$cume.pct.of.total*sum(actual))~c(0, gain1$cume.obs), type = "l", xlab = "Cases", ylab = "Cumulative MEDV", main = "Lift Chart for backwards")
segments(0, 0, nrow(b.valid.df), sum(actual), lty = "dashed", col = "red", lwd = 2)
#lift for forward
gain2 = gains(actual, 
              lm.forward.pred,
              group = 10)

plot(c(0, gain2$cume.pct.of.total*sum(actual))~c(0, gain2$cume.obs), type = "l", xlab = "#Cases", ylab = "Cumulative MEDV", main = "Lift Chart for forward")
segments(0, 0, nrow(b.valid.df), sum(actual), lty = "dashed", col = "red", lwd = 2)

#lift for step
gain3 = gains(actual, 
              lm.step.pred,
              group = 10)

plot(c(0, gain3$cume.pct.of.total*sum(actual))~c(0, gain3$cume.obs), type = "l", xlab = "#Cases", ylab = "Cumulative MEDV", main = "Lift Chart for step")
segments(0, 0, nrow(b.valid.df), sum(actual), lty = "dashed", col = "red", lwd = 2)
lambda <- 10^seq(-3, 3, length = 100)
library(caret)
Ridge <- train(
  MEDV ~., 
  data = b.train.df, 
  method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)
# Model coefficients
coef(Ridge$finalModel, Ridge$bestTune$lambda)

# Make predictions
predictions_r <- Ridge %>% predict(b.valid.df)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions_r, b.valid.df$MEDV),
  Rsquare = R2(predictions_r, b.valid.df$MEDV)
)

Lasso <- train(
  MEDV ~., 
  data = b.train.df, 
  method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)

coef(Lasso$finalModel, Lasso$bestTune$lambda)

predictions_l <- Lasso %>% predict(b.valid.df)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions_l, b.valid.df$MEDV),
  Rsquare = R2(predictions_l, b.valid.df$MEDV)
)

Elastic <- train(
  MEDV ~., 
  data = b.train.df, 
  method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Model coefficients
coef(Elastic$finalModel, Elastic$bestTune$lambda)

# Make predictions
predictions_e <- Elastic %>% predict(b.valid.df)
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions_e, b.valid.df$MEDV),
  Rsquare = R2(predictions_e, b.valid.df$MEDV)
)

# comparing models
models <- list(ridge = Ridge, lasso = Lasso, elastic = Elastic)
resamples(models) %>% summary( metric = c("RMSE","Rsquare"))


# QUESTION 2 Predicting Average Expenses of an Individual

library(leaps)
library(forecast)
set.seed(1)

housing.df <- read.csv("credit_cards.csv")
train.rows <- sample(rownames(housing.df), dim(housing.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(housing.df), train.rows), dim(housing.df)[1]*0.3)
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))
train.data <- housing.df[train.rows, ]
valid.data <- housing.df[valid.rows, ]
test.data <- housing.df[test.rows, ]
car.lm <- lm(Avgexp ~ ., data = train.data)

# use options () to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)
search <- regsubsets(Avgexp ~ ., data = train.data, nbest = 1, nvmax = dim(train.data)[2], method = "exhaustive") 
sum<-summary(search)
sum

# show models
sum$which

# show matrices
sum$rsq
sum$adjr2
sum$cp

car.lm.step <-step(car.lm, direction = "backward")
summary(car.lm.step) # which variables did it drop?
#Backward Elimination Accuracy
car.lm.step.pred <- predict(car.lm.step, valid.data)
accuracy(car.lm.step.pred, valid.data$Avgexp)


#Forward Selection   for reducing predictors
car.lm.null <- lm(Avgexp~1, data = train.data)
# Use step() to run forward selection
car.lm.step <- step(car.lm.null, scope = list(lower=car.lm.null, upper=car.lm), direction = "forward")
summary(car.lm.step) # which variables were added
car.lm.step.pred <- predict(car.lm.step, valid.data)
accuracy(car.lm.step.pred, valid.data$Avgexp)


#Stepwise for both the directions
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step)# Which variables were added/dropped?
car.lm.step.pred <- predict(car.lm.step, valid.data)
accuracy(car.lm.step.pred, valid.data$Avgexp)


#problem 10.1
#a
bank <- read.csv("banks.csv")
model=glm(Financial.Condition~TotExp.Assets+TotLns.Lses.Assets,family=binomial,data=bank)
out<-summary(model)
out
model$coef[1]
model$coef[2]
model$coef[3]


#b
bank <- read.csv("banks.csv")
model=glm(Financial.Condition~TotExp.Assets+TotLns.Lses.Assets,family="binomial",data=bank)
out<-summary(model)
out
model$coef[1]
model$coef[2]
model$coef[3]

prob<-predict(model, newdat=data.frame(TotExp.Assets=0.11, TotLns.Lses.Assets=0.6),type="response")
prob

odds=prob/(1-prob)
odds

logit=log(odds)
logit

cutoff<-0.5
classify<-ifelse(prob>=cutoff,1,0)
classify


#c
cutoff < as.numeric(0.5)
odds <- cutoff/(1-cutoff)
odds
logit<-log(odds)
logit

#d
model$coef[3]

#e
prob=predict(model,type = "response")
data.new<-cbind(bank,prob)
pred_class<-ifelse(prob>0.5,1,0)
data.new<-cbind(data.new,pred_class)
data.new
t1<-table(data.new$Financial.Condition,data.new$pred_class)
t1

#problem 10.3

#b
df<- read.csv("RidingMowers.csv")
library(ggplot2)
#scatter
plot(df$Lot_Size ~ df$Income,
     main="Scatter Plot",
     ylab="Income",
     xlab="Lot Size",
     col = c('red','green'),
     pch=19,
     cex= 1, cex.lab= 1, cex.axis=1
)
#c
owners_data <- read.csv("RidingMowers.csv")
owners_data$X <- NULL
owners_data$outcome<-ifelse(owners_data$Ownership=="Owner",1,0)
owners_data$outcome<-as.factor(as.character(owners_data$outcome))
str(owners_data)
owners_data$Ownership <- NULL
log.Reg<-glm(outcome~.,data = owners_data,family = "binomial")
summary(log.Reg)
glm(formula = outcome ~ ., family = "binomial", data = owners_data)
prob<-predict(log.Reg,type = "response")
prob
data.new<-cbind(owners_data,prob)
pred_class<-ifelse(prob>0.5,1,0)
data.new<-cbind(data.new,pred_class)
data.new
t1<-table(data.new$outcome,data.new$pred_class)
t1


#d
pred_class1<-ifelse(prob>0.4,1,0)
data.new<-cbind(data.new,pred_class1)
t2<-table(data.new$outcome,data.new$pred_class1)
t2



#e  Odds for calculating income=60k, lotsize = 20,000 sqft
odds <- exp(-25.9382+(0.1109*60)+(0.9638*20))
odds

#Q3 - Salmons.xlsx

#a
data.df<-read.csv("Salmons.csv")
train.rows <- sample(rownames(data.df), dim(data.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(data.df), train.rows), dim(data.df)[1]*0.3)
test.rows <- setdiff(rownames(data.df), union(train.rows, valid.rows))
train.data <- data.df[train.rows, ]
valid.data <- data.df[valid.rows, ]
test.data <- data.df[test.rows,]
model <- glm(data.df$Coupon ~data.df$Spending+data.df$Card,family=binomial,data=train.data)
model

#b

data.df<-read.csv("Salmons.csv")
train.rows <- sample(rownames(data.df), dim(data.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(data.df), train.rows), dim(data.df)[1]*0.3)
test.rows <- setdiff(rownames(data.df), union(train.rows, valid.rows))
train.data <- data.df[train.rows, ]
valid.data <- data.df[valid.rows, ]
test.data <- data.df[test.rows,]
model <- glm(data.df$Coupon ~data.df$Spending+data.df$Card,family=binomial,data=valid.data)
model
pred_test <- predict(model,test.data,type="response")
pred_test
library(gains)
#creating decile lift chart
gain <- gains(data.df$Coupon,pred_test)
barplot(gain$mean.resp / mean(data.df$Coupon), names.arg = gain$depth, xlab = "Percentile",ylab = "Mean Response", main = "Decile-wise lift chart")


#c
library(pROC)
data.df<-read.csv("Salmons.csv")
train.rows <- sample(rownames(data.df), dim(data.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(data.df), train.rows), dim(data.df)[1]*0.3)
test.rows <- setdiff(rownames(data.df), union(train.rows, valid.rows))
train.data <- data.df[train.rows, ]
valid.data <- data.df[valid.rows, ]
test.data <- data.df[test.rows,]
model <- glm(data.df$Coupon ~data.df$Spending+data.df$Card,family=binomial,data=valid.data)
model
pred_test <- predict(model,test.data,type="response")
pred_test
r <- roc(data.df$Coupon, pred_test)
plot.roc(r)

# compute auc
auc(r)
