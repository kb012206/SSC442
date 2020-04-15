library(ggplot2)

# basic bar graph to start
par(mfrow=c(1,2))
ggplot(STATE_CRIME_DATA, aes(fill=State, x=Date, y=Violent))+ 
  geom_bar(position="dodge", stat="identity")
ggplot(STATE_CRIME_DATA, aes(fill=State,x=Date,y=Nonviolent))+
  geom_bar(position="dodge",stat='identity')

# scatter plot with log violent crimes per state

ggplot(STATE_CRIME_DATA,aes(x=as.numeric(Date), y=Violent,color=State))+
  geom_point()+scale_x_log10()+geom_smooth(formula=y~poly(x,1.5),,se=FALSE)

# scatter plot with log violent crimes per state subset 2014

stats_2014 = subset(STATE_CRIME_DATA, format.Date(Date, "%y")=="14")
ggplot(stats_2014,aes(x=as.numeric(Date), y=Violent,color=State))+
  geom_point()+scale_x_log10()+geom_smooth(formula = y ~ poly(x, 1),se=FALSE)+
  labs(x="Date")

# log of date and temp on same axis for Virginia

temps = na.omit(STATE_CRIME_DATA)
ggplot(temps)+
  geom_line(aes(x=as.numeric(Date),y=F_temp), color="red")+
  geom_line(aes(x=as.numeric(Date),y=Violent),color="black")+
  scale_x_log10()

# scatter subplots
install.packages("cowplot")
library(cowplot)

corr_eqn <- function(x,y, digits = 2) {
  corr_coef <- round(cor(x, y), digits = digits)
  paste("italic(r) == ", corr_coef)
}

a <- ggplot(temps, aes(x=F_temp, y=Violent))+
  geom_point()+geom_smooth(method=lm,se=FALSE)+
  labs(title='Violent Crime (Virginia)',x='Temperature (F)', y='Number of reported crimes')+
  geom_text(x = 3, y = 3,
          label = corr_eqn(temps$F_temp,
                           temps$Violent), parse = TRUE)


b <- ggplot(temps, aes(x=F_temp,y=Nonviolent))+
  geom_point()+geom_smooth(method=lm,se=FALSE)+
  labs(title='Non-Violent Crime (Virginia)',x='Temperature (F)', y='Number of reported crimes')+
  geom_text(x = 3, y = 3,
          label = corr_eqn(temps$F_temp,
                           temps$Nonviolent), parse = TRUE)

plot_grid(a, b, labels = "AUTO")
  
