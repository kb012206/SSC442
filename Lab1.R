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
p <- ggplot(data = gapminder,
+             mapping = aes(x = gdpPercap, y = lifeExp, color = 'yellow'))
> p + geom_point() + scale_x_log10()
> p <- ggplot(data = gapminder,
+             mapping = aes(x = gdpPercap, y = lifeExp))
> p + geom_point(color = "yellow") + geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + scale_x_log10()
> p <- ggplot(data = gapminder,
+             mapping = aes(x = gdpPercap, y = lifeExp, color = 'yellow'))
> p + geom_point(color = 'yellow') + scale_x_log10()
> #Make sure to include the color = yellow command inside the geom_point function to make the actual points yellow
> p <- ggplot(data = gapminder,
+             mapping = aes(x = gdpPercap, y = lifeExp))
> p + geom_point() + geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + scale_x_log10()
> #color in geom_Smooth makes the regression line orange, se is turned off so no error is shown, size is for how thick the line should be, and method is linear modeling 
> p + geom_point(alpha = 0.3) +
+     geom_smooth(method = "gam") +
+     scale_x_log10(labels = scales::dollar) +
+     labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
+          title = "Economic Growth and Life Expectancy",
+          subtitle = "Data Points are country-years",
+          caption = "Source: Gapminder")
> library(scales)

Attaching package: ‘scales’

The following object is masked from ‘package:purrr’:

    discard

The following object is masked from ‘package:readr’:

    col_factor

Warning message:
package ‘scales’ was built under R version 3.5.3 
> p <- ggplot(data = gapminder,
+             mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))
> p + geom_point()
> p + geom_point() + scale_x_log10(labels = dollar)
> p + geom_point() + scale_x_log10(labels = dollar) + geom_smooth()
`geom_smooth()` using method = 'loess' and formula 'y ~ x'
> #Fill continent essentially colors in the disparity of points away from the regression line to show error
> p <- ggplot(data = gapminder,
+             mapping = aes(x = gdpPercap, y = lifeExp))
> p + geom_point(mapping = aes(color = continent)) + geom_smooth() + scale_x_log10()
`geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
> 
> #There's a single line because geom_smooth is outside the overall mapping data code so there's only one line for all the different continents
> p <- ggplot(data = gapminder,
+             mapping = aes(x = gdpPercap, y = lifeExp))
> p + geom_point(mapping = aes(color = continent)) +
+     geom_smooth(mapping = aes(color = continent, fill = continent)) +
+     scale_x_log10() +
+     geom_smooth(mapping = aes(color = continent), method = "gam")
`geom_smooth()` using method = 'loess' and formula 'y ~ x'
> #This code is messy because the aes(color=continent) makes all the different continents and their error filled in overlap and confuse the coder so putting it at the top would remove the fill and make it neater
