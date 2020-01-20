library(tidyverse)
library(ggplot2)

# 2
bank <- read.csv("C:/Users/mcgui/Downloads/bank.csv")
View(bank)

p <- ggplot(data = bank,
            mapping = aes(x=age,y=balance, colour = y))
p + geom_point() +  scale_x_log10()
p + geom_smooth() + ggtitle('Age and Existing Balance Affect Subscription Acceptances')

p <- ggplot(data = bank,
            mapping = aes(x=job, fill = y))
p + geom_bar() + coord_flip() + ggtitle('How did different occupations affect program subscription?')


