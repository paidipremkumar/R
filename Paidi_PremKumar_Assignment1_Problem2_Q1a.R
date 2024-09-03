  library(ggplot2)
  # Basic scatter plot
  dataframe <- read.csv("car_insurance.csv")
  set.seed(1234)
  s <- sample(row.names(dataframe),24)
  dataframe[s,]
  dim(dataframe)
  ggplot(dataframe, aes(y=Total_Claim_Amount, x=Income, shape=Response, color=Response)) +geom_point()+ geom_smooth(method=lm, se=FALSE, fullrange=FALSE)
