#I collaborated with Rebecca LaRochelle, Sian Smith, Kayla Coffman, Megan Cozort, Makayla Ball, Spencer Cutlip, and Mackenzie Miller for parts of this assignment.
setwd("~/Desktop/Evolution/Tasks/Task_10")
library(phytools)
#QUESTIONS 1-3
trees <- list()
births <- c()
Fractions <- c()
NetDR <- c()
SpeciationRate <- c()
AvgBranchLength <- c()
for(i in 1:100) {
  births[i] <- runif(1)
  Fractions[i] <- runif(1)
  trees[[i]] <- pbtree(b=births[i], d=births[i]*Fractions[i], n=100)
  NetDR[[i]] <- births[i]-births[i]*Fractions[i]
  SpeciationRate[[i]] <- births[i]
  AvgBranchLength[[i]] <- mean(trees[[i]]$edge.length)
}
#QUESTION 4
births
Fractions
VectorNetDR <- unlist(NetDR)
LogTotalTips <- log(sapply(trees, Ntip))
plot(VectorNetDR, LogTotalTips, main="Relationship between NetDR and Log of Total Tips")
line <- lm(LogTotalTips ~ VectorNetDR)
abline(line)
cor.test(VectorNetDR, LogTotalTips)
#There is a positive correlation between Net DR and Log of Total Tips. Cor value was 0.25.
#QUESTION 5
plot(SpeciationRate, AvgBranchLength, main="Relationship between Speciation Rate and Average Branch Lengths")
VectorAvgBranchLength <- unlist(AvgBranchLength)
VectorSpeciationRate <- unlist(SpeciationRate)
abline(lm(VectorAvgBranchLength ~ VectorSpeciationRate))
#As Speciation Rate increases, Average Branch Length tends to decrease.
#QUESTION 6
cor(VectorAvgBranchLength, VectorSpeciationRate)
#There is a negative correlation between Average Branch Length and Speciation Rate. Cor value was -0.48.
#QUESTION 7
BiggestTree <- trees[[which(LogTotalTips == max(LogTotalTips))]]
plot.phylo(BiggestTree, cex=0.25)
rates <- c()
traits <- list()
MeanOfTraits <- c()
VarianceOfTraits <- c()
for(i in 1:100) {
  rates[i] <- runif(1)
  traits[[i]] <- fastBM(tree = BiggestTree, sig2 = rates[i])
  MeanOfTraits[[i]] <- mean(traits[[i]])
  VarianceOfTraits[[i]] <- var(traits[[i]])
}
#QUESTION 8
typeof(MeanOfTraits)
VectorMeanOfTraits <- unlist(MeanOfTraits)
plot(VectorMeanOfTraits, rates, main = "Relationship Between Mean of Traits and Rates")
abline(lm(rates ~ VectorMeanOfTraits))
cor(VectorMeanOfTraits, rates)
#Correlation value is 0.02, which indicates an extremely slight positive relationship between Mean of Traits and Rates, but probably not enough of a relationship to mean anything.
#QUESTION 9
VectorVarianceOfTraits <- unlist(VarianceOfTraits)
plot(VectorVarianceOfTraits, rates, main= "Relationship Between Variance of Traits and Rates")
abline(lm(rates ~ VectorVarianceOfTraits))
cor(VectorVarianceOfTraits, rates)
#There is a relatively strong correlation between Variance of Traits and Rates. The cor value was 0.63.
#QUESTION 10
traitMat <- cbind(traits[[1]], traits[[2]])
traitMat
plot(traits[[1]], traits[[2]], main="Relationship Between First and Second Element of Traits")
abline(lm(traits[[1]] ~ traits[[2]]))
cor(traits[[1]], traits[[2]])
#There is a weak negative correlation between the 1st and 2nd element of traits. The cor value was-0.20.
##EXTRA CREDIT
?phylomorphospace
phylomorphospace(BiggestTree, traitMat, main="Trait 1 and Trait 2 Phylomorphospace Plot", xlab="Trait 1", ylab="Trait 2")




