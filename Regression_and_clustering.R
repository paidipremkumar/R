

#REGRESSION
# setwd()

# Install libraries neuralnet and MASS (if you need to try various datasets)
install.packages("neuralnet")
library(neuralnet)

####################################################################

###  Regression Model using Neural Network using R       

###################################################################

# Dataset: BostonHousing

set.seed (1234)

bostondata.df <- read.csv("BostonHousing.csv")
str(bostondata.df)

# Normalize Data

max_boston <- apply(bostondata.df,2,max)
min_boston <- apply(bostondata.df,2,min)
boston_scaled.df <- scale(bostondata.df, center = min_boston, scale = max_boston - min_boston)

max_boston
min_boston
head(boston_scaled.df)

# since we are building a regression model, we should not be useing the "CAT..MEDV"
# so, remove the variable "CAT..MEDV"

boston_scaled.df <- boston_scaled.df[, - 14] # removes the variable CAT.MEDV
head(boston_scaled.df)

# Sample Data

index = sample(1:nrow(bostondata.df), round(0.7*nrow(bostondata.df)))
train_boston <- as.data.frame(boston_scaled.df[index,])
valid_boston <- as.data.frame(boston_scaled.df[-index,])

dim(train_boston) # Note that the above sampling method used "rounding"
dim(valid_boston) 

head(train_boston)
head(valid_boston)

data_names <- names(boston_scaled.df)

# Build an NNW model and plot it 

boston_nw1 <- neuralnet(MEDV ~ ., data = train_boston, hidden = 10, linear.output = T)
plot(boston_nw1)

# Apply NNW model to teh validation data

predict_boston <- compute(boston_nw1, valid_boston)

predict_boston$net.result # Shows the outputs -- but these are in scaled units

# To find the accuracy of the model, # We need to convert it back to the original units

# We need to multiply the results by [Max - Min]and then add Min


multiplier <-max(bostondata.df$MEDV) - min(bostondata.df$MEDV)

# test that this works.



# Now, apply the multiplier to the output of predict_boston$net.result

predicted_valid_boston <- predict_boston$net.result * multiplier + min(bostondata.df$MEDV)

predicted_valid_boston # These are the predicted MEDVs for the validation data

# Now apply the same for the scaled validation data

valid_boston_MEDV <- as.data.frame((valid_boston$MEDV)*multiplier + min(bostondata.df$MEDV))

valid_boston_MEDV

# Now, find the MSE (sum of square of errors/no of observation)

MSE_valid_boston_nnw <- sum((valid_boston_MEDV - predicted_valid_boston)^2)/nrow(valid_boston_MEDV)


MSE_valid_boston_nnw

# This is the error using Neural Network.

## --------- Run linear regression model -----(in scaled or original units)------
# Here, we use the scaled data

boston_reg <- lm(MEDV ~., data = train_boston)
summary(boston_reg)

predicted_valid_boston_lm <- predict(boston_reg, valid_boston)

# Convert back to original units using the multiplier

predicted_valid_boston_reg <- predicted_valid_boston_lm * multiplier + min(bostondata.df$MEDV)

predicted_valid_boston_reg # These are the predicted MEDVs for the validation data

# We have already converted the valid data to original units earlier; so, we will use that

# Now, to find the MSE


MSE_valid_boston_reg <- sum((valid_boston_MEDV - predicted_valid_boston_lm)^2)/nrow(valid_boston_MEDV)


MSE_valid_boston_reg

# So, we can now compare the NNW and Regression Models

MSE_valid_boston_nnw
MSE_valid_boston_reg

# Notice that the NNW provides higher accuracy!
-------------------------------------------------------------
  # Suggested Exercises:
  # 1. Try NNW models with different number of nodes
  # 2. Build a Regression Tree model -- what are the top 4 variables to use?
  # 3. Use these 4 variables to build a linear regression model
  # 4. Use these 4 variables to build a Neural Network regression model
  # 5. Compare the NNW, CART, and Linear Regression models -- 
  # 4. Which is the best model for deployment? Explain Why?
  
  #HIERARICHAL CLUSTERING
  # Chapter - 15: Cluster Analysis
  

# Set the working directory first before proceeding further

# Read and review data
utilities.df <- read.csv("Utilities.csv")
summary(utilities.df)

# ------------------Fig 15.1: Scatter Plot of Fuel Cost Vs Sales ----------------------------------

plot(utilities.df$Fuel_Cost ~ utilities.df$Sales,
     xlab="sales",ylab="FuelCost", xlim=c(2000,20000))
text(x=utilities.df$Sales,y=utilities.df$Fuel_Cost,
     labels=utilities.df$Company,pos=4,cex = 0.8,srt=20,offset = 0.2)

# Alternative with ggplot

library(ggplot2)
ggplot(utilities.df,aes(y=Fuel_Cost,x=Sales))+geom_point()+
  geom_text(aes(label=paste("",Company)),size=4,hjust=0.0,angle=15)+
  ylim(0.25,2.25)+xlim(3000,18000)


# -------Table 15.2: R-Code for computing distance between records 

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]
# Now rownames(utilities.df) will show that row names are changed to names of utilities, instead of 1, 2, 3, 4 ,...
# The he dataframe will now have row names instead of 1,2,3, etc.

# So, now we can remove the "company" column
utilities.df <- utilities.df[,-1]
summary(utilities.df)

# The dimension is 22 X 8 instead of 22 X 9
# Verify this by using dim(utilities.df) and names(utilities.df),etc.
# 
# ------------- compute Euclidean distance --------------
# (to compute other distance measures, change the value in method = )
# "dist" function computes Distance Matrix. Check syntax with ?dist
# This function computes and returns the distance matrix computed by 
# using the specified distance measure to compute the distances between the rows of a data matrix
# Usage is: dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

d <- dist(utilities.df, method = "euclidean")
d

# Note that it also prints the row names.

##### ---------------Table 15.4: Code for normalizing data and computing distances--------

# normalize input variables

utilities.df.norm <- sapply(utilities.df, scale)

# ?scale --> Scaling and Centering of Matrix-like Objects
# scale is generic function whose default method centers and/or scales the columns of a numeric matrix.
# Scale Usage: scale(x, center = TRUE, scale = TRUE)
# Compare utilities.df and utilities.df.norm --> The latter are normalized values
# You can look at data utilities.df.norm --> It has 0 mean.

summary(utilities.df.norm)

# Exercise -- Verify that standard deviation = 1

# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

# compute normalized distance based on variables Sales (Column 6) and FuelCost (column 8)
d.norm <- dist(utilities.df.norm[,c(6,8)], method = "euclidean")
d.norm

# ------------------Exercise-1:  Plot the normalized Fuel COst Vs Sales ---------
# ----------------- Exercise-2: Write code to recreate Table 15.3 ------------



#-----------------------------------------------------------



##########################################
#
# Hierarchical Clustering
#
##########################################
#

# ----- Figure 15.3: Code for running hierarchical clustering and generating a dendogram ----

# compute normalized distance based on all 8 variables

d.norm <- dist(utilities.df.norm, method = "euclidean")
d.norm

# Clustering 
# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
# Using single method

hc_single <- hclust(d.norm, method = "single")
plot(hc_single, hang = -1, ann = FALSE)
rect.hclust(hc_single, k=2, border = "red") # Cut the dendrogram into 2 clusters

#  parameter hang = 1 causes x-axis labels to hang below the 0 line
#  ann = FALSE turns off annotation (plot and axis titles) 

# -----------------------------
# Using 'average method
#
hc_average <- hclust(d.norm, method = "average")
plot(hc_average, hang = -1, ann = FALSE)
rect.hclust(hc_average, k=2, border = "red") # Cut the dendrogram into 2 clusters
#
#------------------------------------------
#
# Using "complete" method
#
hc_complete <- hclust(d.norm, method = "complete")
plot(hc_complete, hang = -1, ann = FALSE)
rect.hclust(hc_complete, k=3, border = "red") # Cut the dendrogram into 2 clusters

#
# ----------------------------------------
#
# Median method
#
hc_median <- hclust(d.norm, method = "median")
plot(hc_median, hang = -1, ann = FALSE)
rect.hclust(hc_median, k=2, border = "red") # Cut the dendrogram into 2 clusters
#
#---------------------------------------
#
# Centroid Method
#
hc_centroid <- hclust(d.norm, method = "centroid")
plot(hc_centroid, hang = -1, ann = FALSE)
rect.hclust(hc_centroid, k=2, border = "red") # Cut the dendrogram into 2 clusters

#

hc_ward <- hclust(d.norm, method = "ward.D")
plot(hc_ward, hang = -1, ann = FALSE)
rect.hclust(hc_ward, k=4, border = "red") # Cut the dendrogram into 2 clusters
# ---------------------------------------------

##

## -----------Table 15.6: Computing Cluster Membership by cutting the dendogram -------

memb <- cutree(hc_single, k = 6)
memb

memb <- cutree(hc_average, k = 6)
memb


# Add cluster membership to the dataset

cluster_average <-  memb 

data_with_cluster_average <-cbind(utilities.df,cluster_average)

View(data_with_cluster_average)

# --------------------- 


## -------- ------------- ------------------


## ------------Figure 15.4: Code for creating  heatmap  -----------

# 

# set labels as cluster membership and utility name

row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df), sep = "")

# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))




## ---------- plotting is validated ------ write code to plot color heatmap  -------------------
#
# End of Hierarchical Clustering Illustration

#  ----------  Suggested Exercises -------------

# 1. Shuffle data and find clusters. Does this impact cluster membership?
# 2. Create and compare heatmaps for results from other clustering methods. What differences are observed?
# 3. Create a data frame with cluster memberships obtained from each method. Use this dataset and find clusters
#     using the k-means method (k - 3, 4, 5). Compare memberships with hierarchical clustering.

#K MEANS CLUSTERING

# Chapter - 15.5: Cluster Analysis -- K- Means

# Set the working directory first before proceeding further


################################################################
##
### K-Means Clustering 
###
############################################################

#### Table 15.9

# load and preprocess data 
utilities.df <- read.csv("Utilities.csv")
row.names(utilities.df) <- utilities.df[,1]
utilities.df <- utilities.df[,-1]

# normalized distance:
utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df) 

# run kmeans algorithm 
set.seed(2)
km <- kmeans(utilities.df.norm, 6)

# "6" Specifies the desired number of clusters =6 

# show cluster membership
km$cluster



## ---------------------------------------------------

#### Table 15.10: Cluster Centroids and Squared Distances for k-means with k = 6

# centroids
km$centers
# Centroids = each cluster has a vector of variables means

# within-cluster sum of squares
km$withinss
# If a cluster is a singleton, within-cluster distance = 0

# Cluster Size
km$size
# shows number of observations in each cluster

## -------------------------------------------------------------

#### Figure 15.5: Visual Representation (Profle Plot) of Cluster Centroids

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))

# plot centroids
for (i in c(1:6))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))

# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:6)))

## ------------------------------------------------- ------------------

#### Table 15.11; Euclidean Distance Between Cluster Centroids ---

dist(km$centers)
# ----------------------------- -------------



##   ------Fig. 15.6: Scree Plot: Comparing different choices of k in terms of overall average within-cluster distance-
# Note this code is not in the text book

# Lets' say we want to find the best number for cluster k in the range 2 - 15
# Assign the largest value to the variable expt below:

expt <- 15 
wss <- numeric(expt)
for (k in 1:expt) wss[k] <- sum(kmeans(utilities.df.norm, centers = k, nstart = 25)$withinss)
plot(1:expt, wss, type = "b", xlab = "No of Clusters", ylab = "Within Sum of Squares")


### Suggested Exercises

# 1. Based on review of this scree plot, what is the optimum number of clusters (OPT)?  
# 2. Segment the data for for OPT-1, OPT, OPT+1, and compare characteristics of these 3 clusters 

### End of Code for K-Means -----------
