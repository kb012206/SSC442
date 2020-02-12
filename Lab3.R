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

fit_1 = lm(SalePrice ~ LotArea, ameslist)

fit_2 = lm(SalePrice ~ LotArea+GarageArea, ameslist)

fit_3 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea, ameslist)

fit_4 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF, ameslist)

fit_5 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle, ameslist)

fit_6 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
           +MSSubClass, ameslist)

fit_7 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
           +MSSubClass+YearBuilt, ameslist)

fit_8 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
           +MSSubClass+YearBuilt+YearRemodAdd,ameslist)

fit_9 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
           +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1,ameslist)

fit_10 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2,ameslist)

fit_11 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF,ameslist)

fit_12 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF,ameslist)

fit_13 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF,ameslist)

fit_14 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle,ameslist)

fit_15 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition,ameslist)


#Help predict
?predict

predict_lm1 <- predict(fit_1)
predict_lm2 <- predict(fit_2)
predict_lm3 <- predict(fit_3)
predict_lm4 <- predict(fit_4)
predict_lm5 <- predict(fit_5)
predict_lm6 <- predict(fit_6)
predict_lm7 <- predict(fit_7)
predict_lm8 <- predict(fit_8)
predict_lm9 <- predict(fit_9)
predict_lm10 <- predict(fit_10)
predict_lm11 <- predict(fit_11)
predict_lm12 <- predict(fit_12)
predict_lm13 <- predict(fit_13)
predict_lm14 <- predict(fit_14)
predict_lm15 <- predict(fit_15)

#Get RMSE
rmse_lm1 <- rmse(ameslist$SalePrice, predict_lm1)
rmse_lm2 <- rmse(ameslist$SalePrice, predict_lm2)
rmse_lm3 <- rmse(ameslist$SalePrice, predict_lm3)
rmse_lm4 <- rmse(ameslist$SalePrice, predict_lm4)
rmse_lm5 <- rmse(ameslist$SalePrice, predict_lm5)
rmse_lm6 <- rmse(ameslist$SalePrice, predict_lm6)
rmse_lm7 <- rmse(ameslist$SalePrice, predict_lm7)
rmse_lm8 <- rmse(ameslist$SalePrice, predict_lm8)
rmse_lm9 <- rmse(ameslist$SalePrice, predict_lm9)
rmse_lm10 <- rmse(ameslist$SalePrice, predict_lm10)
rmse_lm11 <- rmse(ameslist$SalePrice, predict_lm11)
rmse_lm12 <- rmse(ameslist$SalePrice, predict_lm12)
rmse_lm13 <- rmse(ameslist$SalePrice, predict_lm13)
rmse_lm14 <- rmse(ameslist$SalePrice, predict_lm14)
rmse_lm15 <- rmse(ameslist$SalePrice, predict_lm15)

#Get Complexity
complex_lm1 <- get_complexity(fit_1)
complex_lm2 <- get_complexity(fit_2)
complex_lm3 <- get_complexity(fit_3)
complex_lm4 <- get_complexity(fit_4)
complex_lm5 <- get_complexity(fit_5)
complex_lm6 <- get_complexity(fit_6)
complex_lm7 <- get_complexity(fit_7)
complex_lm8 <- get_complexity(fit_8)
complex_lm9 <- get_complexity(fit_9)
complex_lm10 <- get_complexity(fit_10)
complex_lm11 <- get_complexity(fit_11)
complex_lm12 <- get_complexity(fit_12)
complex_lm13 <- get_complexity(fit_13)
complex_lm14 <- get_complexity(fit_14)
complex_lm15 <- get_complexity(fit_15)

rmse_complexity_plot <- plot(c(complex_lm1,complex_lm2,complex_lm3,
                               complex_lm4,complex_lm5,complex_lm6,
                               complex_lm7,complex_lm8,complex_lm9,
                               complex_lm10,complex_lm11,complex_lm12,
                               complex_lm13,complex_lm14,complex_lm15),
                             c(rmse_lm1,rmse_lm2,rmse_lm3,rmse_lm4,
                               rmse_lm5,rmse_lm6,rmse_lm7,rmse_lm8,
                               rmse_lm9,rmse_lm10,rmse_lm11,rmse_lm12,
                               rmse_lm13,rmse_lm14,rmse_lm15), 
                             main = "RMSE vs Complexity", xlab = "Model Complexity", ylab = "RMSE")

## Exercise 2 ##

# Test Train Split
set.seed(9)
num_obs = nrow(ameslist)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = ameslist[train_index, ]
test_data = ameslist[-train_index, ]

get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

# Additional models added for increased flexibility
fit_16 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath,ameslist)

fit_17 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr,ameslist)

fit_18 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath,ameslist)

fit_19 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath+BsmtHalfBath,ameslist)

fit_20 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath+BsmtHalfBath+
              KitchenAbvGr,ameslist)

fit_21 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath+HalfBath+
              KitchenAbvGr+Fireplaces,ameslist)

fit_22 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath+HalfBath+
              KitchenAbvGr+Fireplaces+HalfBath,ameslist)

fit_23 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath+HalfBath+
              KitchenAbvGr+Fireplaces+HalfBath+GarageCars,ameslist)

fit_24 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath+HalfBath+
              KitchenAbvGr+Fireplaces+HalfBath+GarageCars+GarageArea,ameslist)

fit_25 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath+HalfBath+
              KitchenAbvGr+Fireplaces+HalfBath+GarageCars+GarageArea+
              WoodDeckSF,ameslist)

fit_26 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath+HalfBath+
              KitchenAbvGr+Fireplaces+HalfBath+GarageCars+GarageArea+
              WoodDeckSF+OpenPorchSF,ameslist)

fit_27 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath+HalfBath+
              KitchenAbvGr+Fireplaces+HalfBath+GarageCars+GarageArea+
              WoodDeckSF+OpenPorchSF+EnclosedPorch,ameslist)
fit_28 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle
            +MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2
            +BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition+
              FullBath+BedroomAbvGr+BsmtFullBath+HalfBath+
              KitchenAbvGr+Fireplaces+HalfBath+GarageCars+GarageArea+
              WoodDeckSF+OpenPorchSF+EnclosedPorch+YrSold,ameslist)

model_list = list(fit_1, fit_2, fit_3, fit_4, fit_5,fit_6, fit_7, fit_8, 
                  fit_9, fit_10,fit_11, fit_12, fit_13, fit_14, fit_15,
                  fit_16,fit_17,fit_18,fit_19,fit_20,fit_21,fit_22,fit_23,
                  fit_24,fit_25,fit_26,fit_27,fit_28)

train_rmse = sapply(model_list, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(model_list, get_rmse, data = test_data, response = "SalePrice")

model_complexity = sapply(model_list, get_complexity)


plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange")
RMSE_table = data.frame(
  Model = c('fit_1','fit_2','fit_3','fit_4','fit_5','fit_6','fit_7',
            'fit_8','fit_9','fit_10','fit_11','fit_12','fit_13','fit_14',
            'fit_15','fit_16','fit_17','fit_18','fit_19','fit_20','fit_21',
            'fit_22','fit_23','fit_24','fit_25','fit_26','fit_27','fit_28'),
  TestRMSE = test_rmse,
  TrainRMSE = train_rmse
)

# 2. Minimize RMSE

plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange") 
legend(22, 75000, legend=c("train RMSE", "test RMSE"),
       col=c("dodgerblue", "darkorange"),lty=1, cex=0.8)

RMSE_table_FINAL = data.frame(
  Model = c('fit_1','fit_2','fit_3','fit_4','fit_5','fit_6','fit_7',
            'fit_8','fit_9','fit_10','fit_11','fit_12','fit_13','fit_14',
            'fit_15','fit_16','fit_17','fit_18','fit_19','fit_20','fit_21',
            'fit_22','fit_23','fit_24','fit_25','fit_26','fit_27','fit_28'),
  TestRMSE = test_rmse,
  TrainRMSE = train_rmse
)
View(RMSE_table_FINAL)

#Check to see if R^2 is in a desirable range
#So we know whether the model is under/over fit at lowest Test RMSE
r2_view <- step(fit_25,direction = "forward")

summary(r2_view)
