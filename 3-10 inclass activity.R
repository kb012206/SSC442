ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
options(na.action='na.omit')

Ames = subset(ameslist, select = -c(OverallCond,OverallQual))

set.seed(9)
num_obs = nrow(Ames)

# split subset into train and test

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]

install.packages('caret', dependencies = TRUE)

library(caret)
x = train_data['Fireplaces']
y = as.numeric(unlist(train_data['SalePrice']))

knn_reg_1 = knnreg(x, y, data = train_data, k = 5, use.all = TRUE)

par(mfrow = c(1, 3))

(plot(train_data, pch = 20, col = "grey", cex = 2,
     main = "k = 25"))
curve(cubic_mean(x), add = TRUE, lwd = 2, lty = 2)
curve(predict(knn_slr_25, tibble(x = x)),
      col = "firebrick", lwd = 2, lty = 1, add = TRUE, n = 10000)
grid()


