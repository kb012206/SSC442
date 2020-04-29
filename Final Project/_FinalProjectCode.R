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
 
#treecover data
id <- c(1:8)
state <- c("Alabama","Arizona","Maine","Oregon","Utah", "Vermont","Virginia", "Washington")
tree <- c(.64,.15,.86,.52,.17,.81,.55,.33)
#Crime averages reported per 100k people in 2018
violcrime <- c(519.6,474.9,112.1,285.5,172,200,311.5,289.9)
nonviolcrime <- c(2817.2,2676.8,1357.8,2894,2377.5,1283.1,1665.5,2946.2)
df <- data.frame(id,state,tree, violcrime, nonviolcrime)
print(df)

#tree cover plot
library(ggplot2)
ggplot(data=df,aes(x=state,y=tree))+geom_bar(stat="identity",fill="blue")


#Temperature regression
ViolentCrime <- lm( Violent ~ F_temp , data = STATE_CRIME_DATA)

NonviolentCrime <- lm( Nonviolent ~ F_temp , data = STATE_CRIME_DATA)

#Tree cover regression
ViolentCrime1 <- lm(violcrime ~ tree, data = df)
NonViolentCrime1 <- lm(nonviolcrime ~ tree, data = df)

summary(ViolentCrime1)
summary(ViolentCrime)
#Scatter plot of average crime rate per stae
ggplot(df, aes(x=state)) + 
  geom_point(aes(y = violcrime)) + 
  geom_point(aes(y = nonviolcrime))
  
summary(NonViolentCrime1)
summary(NonviolentCrime)

