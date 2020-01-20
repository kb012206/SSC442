library(tidyverse)
library(ggplot2)

# 2
bank <- read.csv("C:/Users/mcgui/Downloads/bank.csv")
View(bank)

p <- ggplot(data = bank,
            mapping = aes(x=age,y=balance, colour = education))
p + geom_point() +  scale_x_log10()

p <- ggplot(data = bank,
            mapping = aes(x=education, y=age, fill = y))
p + geom_col() + geom_text(aes(label = age), position = position_stack(vjust = 0.5))


