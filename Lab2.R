##Lab 2##

## Backstory and Set Up - Data Exploration and Processing ##
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE ,
                       sep = ",")

ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = FALSE,
                       sep = ",")
## When we specify header = FALSE, The header row is not recognized and gets moved into the 'actual' data

ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv"
                       sep = ",")

?read.table
## Default behavior of the function: Reads a file in table format and creates a 
## data frame from it, with cases corresponding to lines and variables to fields in the file.

ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE ,
                       sep = ",")

names(ameslist)
#Tells us the variable names of the data

typeof(ameslist)
#Tells us that the data type is a list

unique(ameslist$GarageType)

GarageTemp = model.matrix( ~ ameslist$GarageType - 1, data=ameslist$GarageType )

ameslist <- cbind(ameslist, GarageTemp)
##differing number of rows

nrow(ameslist)
#1461 rows

nrow(GarageTemp)
#1379 rows

## Exercise 1 ##

## Building a Model ##

## Exercise 2 ##
