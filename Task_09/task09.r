setwd("~/Desktop/Evolution/Tasks/Task_09")
#I collaborated with Rebecca LaRochelle, Megan Cozort, Kayla Coffman, Alexis Wood, Sian Smith, and Ashton Hudson for portions of this assignment.
tree2 <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
plot(tree2, type="fan", cex=0.25)
#QUESTION 1
Ntip(tree2)
str(tree2)
#There are 82 tips in this tree, and there are branch lengths present.
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors = F, row.names=1)
#QUESTION 2
typeof(data)
dim(data)
#The data object is a list. The dimensions are 82 rows, and 1 column.
svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(tree2, svl, vars=TRUE, CI=TRUE)
Ancestors
#QUESTION 3
#The estimated values are stored under the $ace element.
#The $CI95 element is an optional part of the function that computes 95% confidence intervals for each one of the estimates it makes.
#Basically, the simulation is 95% confident the actual body size of an ancestor lied somewhere between the lower and upper values it gives.
#QUESTION 4
#Two assumptions made in the estimation of the ancestral states are that the data on the tree are independent, and that branch lengths are present.
par(mar=c(0.1, 0.1, 0.1, 0.1))
plot(tree2, type="fan", lwd=2, show.tip.label = F)
tiplabels(pch=16, cex=0.25*svl[tree2$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree2, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree2)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
head(fossilData)
#QUESTION 5
fossilNodes <- c()
nodeN <- c()
for(i in 1:6) {
Node <- fastMRCA(tree2, fossilData[i, "tip1"], fossilData[i, "tip2"])
fossilNodes[i] <- fossilData[i, "svl"]
nodeN[i] <- Node
}
names(fossilNodes) <- nodeN
head(tree2)
head(fossilData)
i
Ancestors_withFossils <- fastAnc(tree2, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
#QUESTION 7
Ancestors
Ancestors_withFossils
#Fossils change the estimated ancestral sizes by increasing them greatly. With this, the variance also skyrockets, which leads to the lower and upper 95% CIs having a massive difference for each ancestral size. 
#QUESTIONS 8-10
install.packages("geiger")
library(geiger)
lambdaFit <- fitContinuous(tree2, data, model="lambda")
brownianFit <- fitContinuous(tree2, data, model="BM")
ouFit <- fitContinuous(tree2, data, model="OU")
ebFit <- fitContinuous(tree2, data, model="EB")
ratetrendFit <- fitContinuous(tree2, data, model="rate_trend")
kappaFit <- fitContinuous(tree2, data, model="kappa")
deltaFit <- fitContinuous(tree2, data, model="delta")
meantrendFit <- fitContinuous(tree2, data, model="mean_trend")
whiteFit <- fitContinuous(tree2, data, model="white")
lambdaFit
brownianFit
ouFit
ebFit
ratetrendFit
kappaFit
deltaFit
meantrendFit
whiteFit
#The model that best fits this data seems to be the EB model, as it has the lowest AIC.
?fastAnc
#This model is different than what fastAnc assumes. fastAnc assumes a Brownian model.





