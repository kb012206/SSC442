ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

names(ameslist)
typeof(ameslist)

View(ameslist)

unique(ameslist$GarageType)

# create GarageType
GarageType = ameslist$GarageType

# create GarageTemp as model of 

options(na.action='na.pass')

GarageTemp = model.matrix( ~ GarageType - 1, data=ameslist$GarageType)
help(model.matrix)

# First try it was the na.action = na.pass above, to stop model.matrix from removing NA values

ameslist <- cbind(ameslist, GarageTemp)

options(na.action='na.exclue')

ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
GarageOutside = ameslist$GarageOutside
GarOut_NoNa = na.omit(GarageOutside)
unique(GarOut_NoNa)


options('na.action')

View(ameslist)
View(GarageTemp)

# 1. Prune the data
keeps = c('Id', 'MSSubClass', 'LotFrontage', 'LotArea', 'OverallQual', 'OverallCond', 'MasVnrArea', 'GrLivArea','BsmtFullBath','BsmtHalfBath', 'FullBath','HalfBath','BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces','GarageCars', 'GarageArea', 'WoodDeckSF', 'PoolArea', 'MiscVal', 'SalePrice')
Ames = ameslist[keeps]

# b. Scatter plot matrix with 12 variables and SalesPrice
pairs(Ames[,11:22])

# c. create a correlation matrix with these variables
help(cor)
cor(Ames[,11:22])

# d. scatter plot between SalePrice and GrLivArea
library(tidyverse)
library(ggplot2)

p <- ggplot(data = Ames,
            mapping = aes(x = SalePrice, y = GrLivArea))
slope = cor(Ames['GrLivArea'],Ames['SalePrice'])
p + abline(slope,0) + geom_point()
help(abline)
