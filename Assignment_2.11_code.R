# Table 2.6 Create Dummy Variables in R

# Use model.matrix() to convert all categorical variables in the data frame into
# a set of dummy variables. We must then turn the resulting data matrix back into
# a data frame for further work

xtotal <- model.matrix(~ 0 + BEDROOMS + REMODEL, data = housing.df)
xtotal$BEDROOMS[1:5] # Will not work because xtotal is a matrix

xtotal <- as.data.frame(xtotal) # convert matrix to data frame
t(t(names(xtotal))) # check the names of the dummy variables
head(xtotal)
xtotal <- xtotal[, -4] # drop one of the dummy variables
# In this case, we drop the "REMODLERecent" 

