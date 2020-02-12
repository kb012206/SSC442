library(tidyverse)
library(ggplot2)

## Exercise 1 ##

#Importing dataset
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

#Drops variables OverallCond and OverallQual
ameslist$OverallCond <- ameslist$OverallQual <- NULL

#Forward Selection
forward <- step(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd+GarageArea+GrLivArea
                   +TotalBsmtSF+MasVnrArea+LotFrontage+BsmtFinSF1+BsmtFinSF2
                   +BsmtUnfSF+X1stFlrSF+X2ndFlrSF,ameslist),direction = "forward")
#Residual
summary(forward)

#Complexity function
get_complexity = function(model) {
  +     length(coef(model)) - 1
}

#RMSE function
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
  
#Help predict
?predict

predict_lm1 <- predict(lm(SalePrice~ MSSubClass+LotArea, ameslist))

predict_lm2 <- predict(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd, ameslist))

predict_lm3 <- predict(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd+GarageArea+GrLivArea, ameslist))

predict_lm4 <- predict(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd+GarageArea
                          +GrLivArea+TotalBsmtSF+MasVnrArea, ameslist))

predict_lm5 <- predict(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd+GarageArea
                          +GrLivArea+TotalBsmtSF+MasVnrArea+LotFrontage+BsmtFinSF1, ameslist))

predict_lm6 <- predict(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd+GarageArea
                          +GrLivArea+TotalBsmtSF+MasVnrArea+LotFrontage+BsmtFinSF1+BsmtFinSF2
                          +BsmtUnfSF, ameslist))

predict_lm7 <- predict(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd+GarageArea
                          +GrLivArea+TotalBsmtSF+MasVnrArea+LotFrontage+BsmtFinSF1+BsmtFinSF2
                          +BsmtUnfSF+X1stFlrSF+X2ndFlrSF, ameslist))
#Get RMSE
rmse_lm1 <- rmse(ameslist$SalePrice, predict_lm1)
rmse_lm2 <- rmse(ameslist$SalePrice, predict_lm2)
rmse_lm3 <- rmse(ameslist$SalePrice, predict_lm3)
rmse_lm4 <- rmse(ameslist$SalePrice, predict_lm4)
rmse_lm5 <- rmse(ameslist$SalePrice, predict_lm5)
rmse_lm6 <- rmse(ameslist$SalePrice, predict_lm6)
rmse_lm7 <- rmse(ameslist$SalePrice, predict_lm7)

#Get Complexity
complex_lm1 <- get_complexity(lm(SalePrice~ MSSubClass+LotArea, ameslist))
complex_lm2 <- get_complexity(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd, ameslist))
complex_lm3 <- get_complexity(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd
                                 +GarageArea+GrLivArea, ameslist))
complex_lm4 <- get_complexity(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd+GarageArea
                                 +GrLivArea+TotalBsmtSF+MasVnrArea, ameslist))
complex_lm5 <- get_complexity(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd+GarageArea
                                 +GrLivArea+TotalBsmtSF+MasVnrArea+LotFrontage+BsmtFinSF1, ameslist))
complex_lm6 <- get_complexity(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd+GarageArea
                                 +GrLivArea+TotalBsmtSF+MasVnrArea+LotFrontage+BsmtFinSF1+BsmtFinSF2
                                 +BsmtUnfSF, ameslist))
complex_lm7 <- get_complexity(lm(SalePrice~ MSSubClass+LotArea+YearBuilt+YearRemodAdd+GarageArea
                                 +GrLivArea+TotalBsmtSF+MasVnrArea+LotFrontage+BsmtFinSF1+BsmtFinSF2
                                 +BsmtUnfSF+X1stFlrSF+X2ndFlrSF, ameslist))

rmse_complexity_plot <- plot(c(complex_lm1,complex_lm2,complex_lm3,complex_lm4,complex_lm5,complex_lm6,complex_lm7),
                            c(rmse_lm1,rmse_lm2,rmse_lm3,rmse_lm4,rmse_lm5,rmse_lm6,rmse_lm7), main = "RMSE vs Complexity", xlab = "Model Complexity", ylab = "RMSE")
