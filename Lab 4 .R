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







