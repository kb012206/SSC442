ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

names(ameslist)
typeof(ameslist)

View(ameslist)

unique(ameslist$GarageType)

# create GarageType
GarageType = ameslist$GarageType

# create GarageTemp as model of 
GarageTemp = model.matrix( ~ GarageType - 1, data=ameslist$GarageType)
help(model.matrix)
ameslist <- cbind(ameslist, GarageTemp)
View(GarageTemp)
