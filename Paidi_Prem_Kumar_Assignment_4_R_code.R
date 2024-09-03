#Part 1: Association Rules
#Problem 14.4
install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)

cosmetics.df <- read.csv("Book1.csv")

class(cosmetics.df) # This shows that this is a data frame
str(cosmetics.df) # This will show the binary version of the faceplate data by column names
View(cosmetics.df)

# remove first column and convert data frame to matrix
cosmetics.mat <- as.matrix(cosmetics.df[, -1])
View(cosmetics.mat)

# convert the binary incidence matrix into a transactions database (list format)
cosmetics.trans <- as(cosmetics.mat, "transactions") # This converts to "transaction" class (e.g., binary matrix or data frame)
cosmetics.trans
inspect(cosmetics.trans) # This shows the data in list format
cosmetics.trans@itemInfo # provides the items in the sataset -- there are 6 items

## get rules
# when running apriori(), include the minimum support, minimum confidence, and target as arguments. 

rules <- apriori(cosmetics.trans, parameter = list(supp = 0.6, conf = 0.8, target = "rules")) # --> generates 18 rules
inspect(sort(rules,by="lift"))

# plot(rules) -- not in Shmueli book

plot(rules,jitter=0)
# inspect the first six rules, sorted by their lift
inspect(head(sort(rules, by = "lift"), n = 6))

#b ii)
inspect(head(sort(rules, by="lift"),1))
colSums(cosmetics.df[,-1])
BrushesNail.Polish <- cosmetics.df$Brushes + cosmetics.df$Nail.Polish
as.data.frame(table(BrushesNail.Polish))

#Problem 2
install.packages("arules")
install.packages("arulesViz")

library(arules)
library(arulesViz)

titanic.df <- read.csv("titanicdata.csv")
titanic.df <- titanic.df[,-1]
#default aasocaition rules
rules <- apriori(titanic.df,appearance = list(default="lhs", 
                                              rhs=c("Survived=No","Survived=Yes")))
sort.rule <- sort(rules, by="lift")
inspect(sort.rule)

rules <- apriori(titanic.df,parameter = list( supp=0.003, conf=0.8),appearance = list(default = "lhs",rhs=c("Survived=No", "Survived=Yes")),control = list(verbose=F))
inspect(sort(rules, by = "lift"))

#Part 2 k means Clustering

Labs.df <- read.csv("Labs.csv") #To read the dataset
Labs.df<-na.omit(Labs.df) 
row.names(Labs.df) <- Labs.df[,1]
Labs.df<- Labs.df[,-1]
# To normalize input variables
labs.df.norm <- sapply(Labs.df, scale)
row.names(labs.df.norm) <- row.names(Labs.df)
set.seed(1234)
plot(Labs.df[,c("ROE","Beta","Market_Cap","PE_Ratio")])
km <- kmeans(labs.df.norm, 3)
km$cluster
km$centers
km$withinss
km$size

plot(c(0), xaxt = 'n', ylab = "", type = "l",ylim = c(min(km$centers), 
                                                      max(km$centers)), xlim = c(0, 9))
# x-axis labeling
axis(1, at = c(1:9), labels = names(labs.df.norm))
# plot centroids using aggregate function
for (i in c(1:3))
  lines(km$centers[i,], lty = i, lwd = 2)
#To find the centers using aggregate function
aggregate(labs.df.norm, by=list(cluster=km$cluster), mean)
