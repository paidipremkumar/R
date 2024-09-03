
#ASSOCIATION RULES


# Chapter - 14 "Association Rules"

# setwd()

# install packages and Load Libraries

install.packages("arules")

library(arules)
library(arulesViz)

#  --------------   Table 14.4  -------------------------

fp.df <- read.csv("Faceplate.csv")

class(fp.df) # This shows that this is a data frame
str(fp.df) # This will show the binary version of the faceplate data by column names
View(fp.df)

# remove first column and convert data frame to matrix
fp.mat <- as.matrix(fp.df[, -1])
View(fp.mat)

# convert the binary incidence matrix into a transactions database (list format)
fp.trans <- as(fp.mat, "transactions") # This converts to "transaction" class (e.g., binary matrix or data frame)
fp.trans
inspect(fp.trans) # This shows the data in list format
fp.trans@itemInfo # provides the items in the sataset -- there are 6 items

## get rules
# when running apriori(), include the minimum support, minimum confidence, and target as arguments. 
rules <- apriori(fp.trans)
rules <- apriori(fp.trans, parameter = list(supp = 0.2, conf = 0.5, target = "rules")) # --> generates 18 rules
inspect(rules)
inspect(head(sort(rules, by = "lift"), n = 6))

# plot(rules) -- not in Shmueli book

plot(rules)
# inspect the first six rules, sorted by their lift
inspect(head(sort(rules, by = "lift"), n = 6))

# -----------------Table 14.6 completed ------------------------

#  ---------------Practice Exercises --------------------------

##   Practice Class Exercises -- Try to find rules with various values of support and confidence
##   Summarize results / findings


# -----------------------------------------------------------------

#ASSOCIATION RULES - CHARLES CLUB

# Chapter - 14 "Association Rules"

# setwd()

# install packages and Load Libraries

install.packages("arules")

library(arules)
library(arulesViz)




# -------------------------- Table 14.8 ----------------------------
all.books.df <- read.csv("CharlesBookClub.csv")
View(all.books.df)

# Data Exploration
# The following explores data
head(all.books.df) # displays 6 records
dim(all.books.df) # displays number of rows (4000) and number of columns(24)
colnames(all.books.df) # displays column names
str(all.books.df) # shows structure of 24 variables - they are all integers

# Create a binary incidence matrix

count.books.df <- all.books.df[, 8:18] # creates a new data frame without columns 1- 7
incid.books.df <- ifelse(count.books.df > 0, 1, 0) # what does this ifelse do?
colnames(incid.books.df)
incid.books.mat <- as.matrix(incid.books.df[, -1]) # eliminates the first column - childbks
colnames(incid.books.mat)

#  convert the binary incidence matrix into a transactions database

books.trans <- as(incid.books.mat, "transactions") # converts binary into list format
inspect(books.trans)

# plot data (Not in Shmueli Book)
itemFrequencyPlot(books.trans) # creates bar charts for item frequency


# Run apriori algorithm (function)

rules <- apriori(books.trans) # Notice that "0" rules are returned.
rules <- apriori(books.trans, 
                 parameter = list(supp= 200/4000, conf = 0.5, target = "rules")) 
# 21 rules are generated

# inspect rules
inspect(sort(rules, by = "lift"))

# --------------------  Table 14.8 completed -------------------------------








