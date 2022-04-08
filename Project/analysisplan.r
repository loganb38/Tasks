setwd("~/Desktop/Evolution/Tasks/Project")
#READ IN DR/BODY MASS DATA
ProjectData <- read.csv("/Users/lcb/Desktop/Evolution/Tasks/Project/Data/finalprojectdata.csv", stringsAsFactors = FALSE)
ProjectData <- ProjectData[,c(1,4,9)]
head(ProjectData)

Model <- lm(ProjectData$DR~ProjectData$BodySize)
###PLOT OF BODY MASS VS DR
pdf("BMvsDRplot.pdf", height=10, width=10)
par(las=1, mar=c(4,5,1,1))
plot(ProjectData$BodySize, ProjectData$DR, pch=1, log="xy", xlab="Log of Body Mass (g)", ylab="Log of DR", main="Relationship Between Bird Species' Body Mass and DR")
dev.off()
abline(Model)
summary(Model)

plot(ProjectData$BodySize, ProjectData$HabitatBreadth, pch=16)
#READ IN TREE DATA
library(ape)
FinalProjectPhylogeny <- read.tree("~/Desktop/Evolution/Tasks/Project/PhylogenyCode")
plot(FinalProjectPhylogeny, cex=0.25)

tree <- FinalProjectPhylogeny

library(BAMMtools)
library(phytools)
###### CONVERT DR DATA TO EDATA
totLen <- max(nodeHeights(tree))
getBranchLength <- function(x, tree){
  tipN <- which(tree$tip.label == x)
  edgeN <- which(tree$edge[,2] == tipN)
  edLen <- tree$edge.length[edgeN]
  return(edLen)
}
absTime <- sapply(ProjectData[,1], getBranchLength, tree=tree)

dummyEData <- data.frame(generation=c(0,rep(0, nrow(ProjectData))), leftchild=c(tree$tip.label[1], ProjectData[,1]), rightchild=c(tree$tip.label[Ntip(tree)], rep(NA, nrow(ProjectData))), abstime = c(0, totLen - as.vector(absTime)), lambdainit = c(mean(ProjectData[,2]), ProjectData[,2]), lambdashift = c(0,rep(0, nrow(ProjectData))), muinit=c(0,rep(0, nrow(ProjectData))), mushift = c(0, rep(0, nrow(ProjectData))))

EData <- getEventData(tree, dummyEData, burnin=0)
#PHYLOGENY WITH DR
plot(EData, breaksmethod="jenks")
x <- plot(EData, breaksmethod="jenks")
addBAMMlegend(x)
#####
#PHYLOGENY WITH BODY MASS (contMap)
library(phytools)
y2 <- setNames(ProjectData$BodySize, ProjectData$Species)
par(mfrow=c(1,1))
contMap(tree, y2, fsize=c(0.1,0.6))
##STRAPP CORRELATION TEST
Trait <- ProjectData[,3]
names(Trait) <- ProjectData[,1]
STRAPPoutput <- traitDependentBAMM(EData, Trait, 100)
STRAPPoutput
#UPDATED HYPOTHESIS 24 MAR 2022: There is no correlation between body mass and diversification rate in the bird species recorded.
#NOTE: I still need to edit my graphs to look better and more professional (add titles, add legends/key, change axes titles, etc.). This will be done by the submission deadline in April. 
##^^^^DONE