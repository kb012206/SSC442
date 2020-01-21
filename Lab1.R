library(tidyverse)
library(ggplot2)
library(gapminder)
gapminder
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()

ggplot2::mpg

#Other geom_ functions: contour, count, density, blank, abline, hex, map

## Exercise 1a ##
ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy))+
  geom_point()
#As engine size increases, you would expect hwy mileage to decrease. The relationship is as expected.

ggplot(data = mpg, 
       mapping = aes(x = class, y = drv))+
  geom_point()
#When we plot these variables, we are being shown cars that have a specific drive (front wheel, rear wheel, 4 wheel)
#but there is no telling the amount of cars that fit into each point as it currently stands.

## Exercise 1b ##
ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy, color = class))+
  geom_point()
#In comparison, we see that compact and midsize vehicles with smaller engines have the highest hwy mileage
#while larger vehicles with larger engines have lower hwy mileage


## Exercise 2 ##
bank <- read.csv("C:/Users/mcgui/Downloads/bank.csv")
View(bank)

p <- ggplot(data = bank,
            mapping = aes(x=age,y=balance, colour = y))
p + geom_point() +  scale_x_log10()
p + geom_smooth() + ggtitle('Age and Existing Balance Affect Subscription Acceptances')

p <- ggplot(data = bank,
            mapping = aes(x=job, fill = y))
p + geom_bar() + coord_flip() + ggtitle('How did different occupations affect program subscription?')
