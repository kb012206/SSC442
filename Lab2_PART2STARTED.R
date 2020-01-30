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

options(na.action='na.exclude')

ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
GarageOutside = ameslist$GarageOutside
GarOut_NoNa = na.omit(GarageOutside)
unique(GarOut_NoNa)


options('na.action')

View(ameslist)
View(GarageTemp)

# 1. Prune the data
keeps = c('Id', 'MSSubClass', 'LotFrontage', 'GarageOutside', 'LotArea', 'OverallQual', 'OverallCond', 'MasVnrArea', 'GrLivArea','BsmtFullBath','BsmtHalfBath', 'FullBath','HalfBath','BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces','GarageCars', 'GarageArea', 'WoodDeckSF', 'PoolArea', 'MiscVal', 'SalePrice')
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

# exercise 2a, run regression on outside garage

attach(Ames)
garage.fit = lm(SalePrice ~ GarageOutside)
garage.fit

# exercise 2b, run regression on SalePrice with all Ames
all.fit = lm(SalePrice ~ ., data = Ames[,-1])
plot(all.fit)
summary(all.fit)
#There does seem to be a relationship between the predictors and the response.
#The multiple r-squared is fairly high which indicates correlation and the estimated coefficients 
#for each variable is large enough for the most part that it does seem to influence the response

#MSSubClass, GarageOutside, LotArea, OverallQual, OverallCond, MasVnrArea, GrLivArea, BsmtFullBath, 
#BsmtHalfBath, FullBath, BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageCars, WoodDeckSF 
#are all important variables since they all have p-values less than .05
all.fit = lm(SalePrice ~ ., data = Ames[,-1])plot(all.fit)
all.fit = lm(SalePrice ~ ., data = Ames[,-1])
#1183, 524, and 1299 all seem to be outliers that show up on the residual plot as well as the residuals vs. leverage one

cor(log(ameslist['LotArea']), ameslist['SalePrice'])
        SalePrice
LotArea 0.3885203

> cor(log(ameslist['GrLivArea']), ameslist['SalePrice'])
          SalePrice
GrLivArea 0.6951181
> cor((ameslist['GrLivArea'])**2, ameslist['SalePrice'])
          SalePrice
GrLivArea 0.6522667
> cor((ameslist['GrLivArea'])**(1/2), ameslist['SalePrice'])
          SalePrice
GrLivArea 0.7087645
> #log of GrLivArea has a better correlation with SalePrice than x^2 but sqrt is even better. 
