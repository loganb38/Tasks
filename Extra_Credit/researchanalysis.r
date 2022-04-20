#I am using R for my research with Dr. B to analyze data I collected throughout the semester on birds.
#I'm running a linear regression test and creating a plot with the regression line to see if temperature and birds observed in my backyard are linked.
#Pearson test was just for fun.
setwd("~/Desktop/Evolution/Tasks/Extra_Credit")
BirdResearchData <- read.csv("~/Desktop/Evolution/Tasks/Extra_Credit/researchdata.csv")
RegLine <- lm(BirdResearchData$Birds.Counted~BirdResearchData$Temperature..Celsius.)
PearsonLine <- cor.test(BirdResearchData$Temperature..Celsius., BirdResearchData$Birds.Counted, method="pearson")
par(mar=c(5, 5, 5, 5))
plot(BirdResearchData$Temperature..Celsius., BirdResearchData$Birds.Counted, 
     main="Relationship Between Temperature and Birds Observed", xlab="Temperature (C)",
     ylab="Birds Observed", pch=16)
abline(RegLine, col="red")
abline(PearsonLine, col="blue")
summary(RegLine)
summary(PearsonLine)
