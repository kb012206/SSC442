## Exercise 1 ##

#importing dataset
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

#Drops variables OverallCond and OverallQual
ameslist$OverallCond <- ameslist$OverallQual <- NULL
