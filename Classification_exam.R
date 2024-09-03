#CLASSIFICATION
# set (wd)

# Install and invoke Library ("neuralnet")

install.packages("neuralnet")
library(neuralnet)
# require(neuralnet)

install.packages("caret")
# read Data
univ.df <- read.csv("UniversalBank Short.csv")
View(univ.df)
str(univ.df)

# Since the target variable is "int", it needs to be converted to a "factor"
Loan <- univ.df$Personal.Loan # Create a column with names
univ.df$Personal.Loan <- as.factor(univ.df$Personal.Loan)
str(univ.df)

# Create a data frame without outcome

univ_numdata.df <- univ.df[,-5] # Remove the Personal Loan Column
View(univ_numdata.df)



# Scale the data for Neural Network ---------------

max = apply(univ_numdata.df,2,max)
max
min = apply(univ_numdata.df,2,min)
min
scalednumdata.df <- as.data.frame(scale(univ_numdata.df, center = min, scale = max-min)) 
View(scalednumdata.df)

# See how the variables are scaled back  -----------
hist(univ.df$Age)
hist(scalednumdata.df$Age)
hist(univ.df$Income)
hist(scalednumdata.df$Income)
# ---------------------------------------


# Build back the data frame with the target variable

univ_scaled.df <- cbind(scalednumdata.df, Loan)
View(univ_scaled.df)

## -----------------------Another way to normalize data -----------
normalize <- function(x) {
  return((x - min(x))/ (max(x) - min(x)))
}

# ------------- -------

# Partition the Data into  a training and validation data set

set.seed(1234) # Try with different seeds 


train.index <- sample(c(1:dim(univ_scaled.df)[1]), dim(univ_scaled.df)[1]*0.6)  
train.df <- univ_scaled.df[train.index, ]
valid.df <- univ_scaled.df[-train.index, ]

# Step - Fit a Neural Network using the normalized data
# Use the neural network library neuralnet() for this purpose
# Syntax of the neuralnet() function is below

# neuralnet(formula, data, hidden = 1, stepmax = 1e+05, rep = 1, lifesign = "none", algorithm = "rprop+",
# err.fct = "sse", linear.output = TRUE)

# ---------------Parameters of neuralnet(): -------------------------------

# formula: a symbolic description of the model to be fitted
# data: a data frame containing the variables specified in the formula
# hidden: a vector of integers specifying the numbers of hidden neurons (vertices) in each layer
# er.fct: a differentiable function that is used for the calculation of error. Alternatively,
# 'sse' (sum of squared errors)  and 'ce' (cross-entropy) can be used
# linear.output: Logical. If act.fct should not be applied to the output neurons set linear output to TRUE, otherwise, false
# lifesign: a string specifying how much the function will print during the calculation of the neural network. "non", "minimal" or 'full'.
# algorithm: a string containing the algorithm type yo calculate the neural network. The following
# types are possible - 'backprop', 'rprop+', rpor-', 'sag', pr 'slr'
# backdrop refers to backpropogation, "rpop+' and 'rprop-' refer to the resilient backpropogation
# with and without backtracking, while 'sag' and 'slr' induce the usage of the modified globally convergent algorithm (grprop)
# stepmax: the maximum steps for the training of the neural network. reaching this maximum leads to a stop of the
# neural network's training process.

# -----------------------------------------------------------

# Now, use the normalized data to Build Neural Network using default values; 

nnloanfull <- neuralnet(Loan ~., data=train.df, hidden = 5, err.fct = "ce", act.fct = "logistic", linear.output = FALSE, lifesign = 'full', rep = 2, algorithm = "rprop+", stepmax = 100000)


# The algorithm did not converge using above -- so, weights will not be calculated.

# Lets try another architecture with just one node

nnloan <- neuralnet(Loan ~., data=train.df, act.fct = "logistic", linear.output = FALSE)


# plot(nnloan)
plot(nnloan, rep = "best") # What does the option rep do? rep = 1?
# The black lines show the connection with weights. The blue line displays the bias term.

# Now, generate the the error of the neural network model, along with the 
# weights between the inputs, hidden layers, and outputs

nnloan$result.matrix

# display weights
nnloan$weights

# Increase the number of nodes to 2 by specifying the number in the option. 
# Notice the reduction in error and increase in number of steps

nnloan2 <- neuralnet(Loan ~., data=train.df, act.fct = "logistic", linear.output = FALSE, hidden = 2)


# plot(nnloan2)
plot(nnloan2, rep = "best") # option rep
# The black lines show the connection with weights. The blue line displays the bias term.


# display weights
nnloan2$weights

# Now, generate the the error of the neural network model, along with the 
# weights between the inputs, hidden layers, and outputs
nnloan2$result.matrix


# Increase the number of nodes to 3 by specifying the number in the option
# Notice the reduction in error and increase in number of steps

nnloan3 <- neuralnet(Loan ~., data=train.df, act.fct = "logistic", linear.output = FALSE, hidden = 3) # might take time to compute


plot(nnloan3, rep = "best") # Option rep
# The black lines show the connection with weights. The blue line displays the bias term.


# display weights
nnloan3$weights

# Now, generate the the error of the neural network model, along with the 
# weights between the inputs, hidden layers, and outputs
nnloan3$result.matrix

# - Step - 4: ---------  Predictions ----------------
# Let us predict the  Loan using this NNW model.

# display predictions
# prediction(nnloan)
#
# prediction(nnloan2)
# prediction(nnloan3)

# --------- hidden = 1 ---------------

outnn1 <- compute(nnloan, rep = 1, train.df)
head(outnn1$net.result)
head(train.df)

# -------------------------------
library(caret)

# Now to find confusion matrix; for hidden = 1  

# The output of neural network running on the training data (as before) is:

outnn1 <- compute(nnloan, rep = 1, train.df)
p1 <- outnn1$net.result # get the probabilities
pred1 <- ifelse(p1>0.5,1,0) # convert probabilities into classification
tabl <- table(pred1, train.df$Loan)
tabl

# Note that this model generates 2662 True Negatives and 130 True Positives; 
# 154 false negatives and 54 false positives [Class 1 is of interest]


# You can also reverse the order in the table 

# tablr <- table(train.df$Loan, pred1)
# tablr


# Let us compute the "Misclassification Error"

error1 <- 1 - sum(diag(tabl)) / sum(tabl)
error1

# This generates the confusion matrix for hidden = 1
# ------------------------------------------------------
# Now to find confusion matrix; for hidden = 2  

outnn2 <- compute(nnloan2, rep = 1, train.df)
head(outnn2$net.result)
head(train.df)

# The output of neural network running on the training data (as before) is:

outnn2 <- compute(nnloan2, rep = 1, train.df)
p2 <- outnn2$net.result # get the probabilities
pred2 <- ifelse(p2>0.5,1,0) # convert probabilities into classification
tab2 <- table(pred2, train.df$Loan)
tab2

# This generates the confusion matrix for hidden = 2
# Let us calculate the Misclassification Error
error2 <- 1 - sum(diag(tab2)) / sum(tab2)
error2
# ----------------------------------

# Now to find confusion matrix; for hidden = 3  

outnn3 <- compute(nnloan3, rep = 1, train.df)
head(outnn3$net.result)
head(train.df)

# The output of neural network running on the training data (as before) is:

outnn3 <- compute(nnloan3, rep = 1, train.df)
p3 <- outnn3$net.result # get the probabilities
pred3 <- ifelse(p3>0.5,1,0) # convert probabilities into classification
tab3 <- table(pred3, train.df$Loan)
tab3

# This generates the confusion matrix for hidden = 3

# Let us calculate the Misclassification Error
error3 <- 1 - sum(diag(tab3)) / sum(tab3)
error3

# ----------------------------------



##------------- Apply different Neural Nets to the Validation Data ----------------


validnn1 <- compute(nnloan, rep = 1, valid.df)
pv1 <- validnn1$net.result # get the probabilities
predv1 <- ifelse(pv1>0.5,1,0) # convert probabilities into classification
tabvl <- table(predv1, valid.df$Loan)
tabvl

# Note that this model generates 1767 True Negatives and 91 True Positives; 
# 105 false negatives and 37 false positives. [Class 1 is of interest]


# You can also reverse the order in the table 

# tablr <- table(train.df$Loan, pred1)
# tablr


# Let us compute the "Misclassification Error"

errorv1 <- 1 - sum(diag(tabvl)) / sum(tabvl)
errorv1

# This generates the confusion matrix for hidden = 1
# ------------------------------------------------------
# Now to find confusion matrix; for hidden = 2  

validnn2 <- compute(nnloan2, rep = 1, valid.df)
pv2 <- validnn2$net.result # get the probabilities
predv2 <- ifelse(pv2>0.5,1,0) # convert probabilities into classification
tabv2 <- table(predv2, valid.df$Loan)
tabv2

# Note that this model generates 1767 True Negatives and 91 True Positives; 
# 105 false negatives and 37 false positives. [Class 1 is of interest]


# You can also reverse the order in the table 

# tablr <- table(train.df$Loan, pred1)
# tablr


# Let us compute the "Misclassification Error"

errorv2 <- 1 - sum(diag(tabv2)) / sum(tabv2)
errorv2

# This generates the confusion matrix for hidden = 2
# ----------------------------------


# Now to find confusion matrix; for hidden = 3  

validnn3 <- compute(nnloan3, rep = 1, valid.df)
pv3 <- validnn3$net.result # get the probabilities
predv3 <- ifelse(pv3>0.5,1,0) # convert probabilities into classification
tabv3 <- table(predv3, valid.df$Loan)
tabv3

# Note that this model generates 1767 True Negatives and 91 True Positives; 
# 105 false negatives and 37 false positives. [Class 1 is of interest]


# You can also reverse the order in the table 

# tablr <- table(train.df$Loan, pred1)
# tablr


# Let us compute the "Misclassification Error"

errorv3 <- 1 - sum(diag(tabv3)) / sum(tabv3)
errorv3

# This generates the confusion matrix for hidden = 3
# ----------------------------------

# Suggested Exercises:

# 1. Try more number of nodes in the hidden layer
# 2. Construct RoC Curve, Lift and Decile Charts
# 3. Try additional hidden layer (caution: Look for overfitting)