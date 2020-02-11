ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
options(na.action='na.omit')

# Exercise 1
# 1. remove two columns from dataset
Ames = subset(ameslist, select = -c(OverallCond,OverallQual))

# 2.
show(Ames)
lm(Neighborhood~SalePrice, data=Ames)

forward = step(lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition,data=ameslist),direction="forward")
summary(forward)


# find RMSE of model
rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

# find complexity of model
get_complexity = function(model) {
  length(coef(model)) - 1
}

# Test Train Split
set.seed(9)
num_obs = nrow(Ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]

# Table for 15 fits
fit_1 = lm(SalePrice ~ LotArea, data = train_data)
fit_2 = lm(SalePrice ~ LotArea+GarageArea, data = train_data)
fit_3 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea, data = train_data)
fit_4 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF, data = train_data)
fit_5 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle, data = train_data)
fit_6 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass,data=train_data)
fit_7 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt,data=train_data)
fit_8 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd,data=train_data)
fit_9 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1,data=train_data)
fit_10 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2,data=train_data)
fit_11 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF,data=train_data)
fit_12 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF,data=train_data)
fit_13 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF,data=train_data)
fit_14 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle,data=train_data)
fit_15 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition,data=train_data)

get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}

model_list = list(fit_1, fit_2, fit_3, fit_4, fit_5,fit_6, fit_7, fit_8, fit_9, fit_10,fit_11, fit_12, fit_13, fit_14, fit_15)

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
  Model = c('fit_1','fit_2','fit_3','fit_4','fit_5','fit_6','fit_7','fit_8','fit_9','fit_10','fit_11','fit_12','fit_13','fit_14','fit_15'),
  TestRMSE = test_rmse,
  TrainRMSE = train_rmse
)

# 2. Minimize RMSE

fit_1 = lm(SalePrice ~ LotArea, data = train_data)
fit_2 = lm(SalePrice ~ LotArea+GarageArea, data = train_data)
fit_3 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea, data = train_data)
fit_4 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+YearBuilt, data = train_data)
fit_5 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+YearBuilt+RoofStyle, data = train_data)
fit_6 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass,data=train_data)
fit_7 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt,data=train_data)
fit_8 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd,data=train_data)
fit_9 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1,data=train_data)
fit_10 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2,data=train_data)
fit_11 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF,data=train_data)
fit_12 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF,data=train_data)
fit_13 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF,data=train_data)
fit_14 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle,data=train_data)
fit_15 = lm(SalePrice ~ LotArea+GarageArea+GrLivArea+TotalBsmtSF+RoofStyle+MSSubClass+YearBuilt+YearRemodAdd+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+HouseStyle+SaleCondition,data=train_data)

plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange")

RMSE_table_FINAL = data.frame(
  Model = c('fit_1','fit_2','fit_3','fit_4','fit_5','fit_6','fit_7','fit_8','fit_9','fit_10','fit_11','fit_12','fit_13','fit_14','fit_15'),
  TestRMSE = test_rmse,
  TrainRMSE = train_rmse
)
View(RMSE_table_FINAL)

