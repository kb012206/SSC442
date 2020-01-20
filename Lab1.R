library(tidyverse)
library(ggplot2)

ggplot2::mpg

#Other geom_ functions: contour, count, density, blank, abline, hex, map

#Exercise 1a
ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy))+
  geom_point()
#As engine size increases, you would expect hwy mileage to decrease. The relationship is as expected.

ggplot(data = mpg, 
       mapping = aes(x = class, y = drv))+
  geom_point()
#When we plot these variables, we are being shown cars that have a specific drive (front wheel, rear wheel, 4 wheel)
#but there is no telling the amount of cars that fit into each point as it currently stands.

#Exercise 1b
ggplot(data = mpg, 
       mapping = aes(x = displ, y = hwy, color = class))+
  geom_point()
