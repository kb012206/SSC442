install.packages("kernlab")
library(kernlab)
data("spam")
tibble::as.tibble(spam)


is.factor(spam$type)
levels(spam$type)


set.seed(42)
# spam_idx = sample(nrow(spam), round(nrow(spam) / 2))
spam_idx = sample(nrow(spam), 1000)
spam_trn = spam[spam_idx, ]
spam_tst = spam[-spam_idx, ]


fit_caps = glm(type ~ capitalTotal,
               data = spam_trn, family = binomial)
fit_selected = glm(type ~ edu + money + capitalTotal + charDollar,
                   data = spam_trn, family = binomial)
fit_additive = glm(type ~ .,
                   data = spam_trn, family = binomial)
fit_over = glm(type ~ capitalTotal * (.),
               data = spam_trn, family = binomial, maxit = 50)


# training misclassification rate
mean(ifelse(predict(fit_caps) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_selected) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_additive) > 0, "spam", "nonspam") != spam_trn$type)
mean(ifelse(predict(fit_over) > 0, "spam", "nonspam") != spam_trn$type)


library(boot)
set.seed(1)
# Model 1
cv.glm(spam_trn, fit_caps, K = 5)$delta[1]
# Model 2
cv.glm(spam_trn, fit_selected, K = 5)$delta[1]
# Model 3
cv.glm(spam_trn, fit_additive, K = 5)$delta[1]
# Model 4
cv.glm(spam_trn, fit_over, K = 5)$delta[1]

## Exercise 1

## 1 - Most underfit to most overfit 

# 1. Model 1 - "fit_caps"
# 2. Model 2 - "fit_selected"
# 3. Model 4 - "fit_over"
# 4. Model 3 - "fit_additive"

## 2

set.seed(3)
# Model 1
cv.glm(spam_trn, fit_caps, K = 100)$delta[1]
# Model 2
cv.glm(spam_trn, fit_selected, K = 100)$delta[1]
# Model 3
cv.glm(spam_trn, fit_additive, K = 100)$delta[1]
# Model 4
cv.glm(spam_trn, fit_over, K = 100)$delta[1]

# Our conclusion stays the same as the results were not any different than our initial analysis 

# Create 4 confusion matrices

make_conf_mat = function(predicted, actual) {
  table(predicted = predicted, actual = actual)
}

spam_add_pred = ifelse(predict(fit_additive, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
spam_caps_pred = ifelse(predict(fit_caps, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
spam_over_pred = ifelse(predict(fit_over, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")
spam_select_pred = ifelse(predict(fit_selected, spam_tst, type = "response") > 0.5,
                       "spam",
                       "nonspam")

(conf_mat_addi = make_conf_mat(predicted = spam_add_pred, actual = spam_tst$type))
(conf_mat_caps = make_conf_mat(predicted = spam_caps_pred, actual = spam_tst$type))
(conf_mat_over = make_conf_mat(predicted = spam_over_pred, actual = spam_tst$type))
(conf_mat_selc = make_conf_mat(predicted = spam_select_pred, actual = spam_tst$type))

table(spam_tst$type) / nrow(spam_tst)

## Exercise 2 ##

bank <- read.csv('https://msudataanalytics.github.io/SSC442/Labs/data/bank.csv')

set.seed(42)
bank_idx <- sample(nrow(bank), 1000)
bank_trn = bank[bank_idx, ]
bank_tst = bank[-bank_idx, ]

bank_log <- glm(y ~., data = bank_trn, family = binomial)

cv <- cv.glm(bank_trn, bank_log, K=10)$delta[1]

#summary(bank_log)
