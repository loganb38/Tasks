setwd("~/Desktop/Evolution/Tasks/Project")
ProjectData <- read.csv("/Users/lcb/Desktop/Evolution/Tasks/Project/Data/finalprojectdata.csv", stringsAsFactors = FALSE)
ProjectData <- ProjectData[,c(1,4,9)]
head(ProjectData)

Model <- lm(ProjectData$DR~ProjectData$BodySize)

par(las=1, mar=c(4,5,1,1))
plot(ProjectData$BodySize, ProjectData$DR, pch=16, log="xy", xlab="Log of Body Mass (g)", ylab="Log of DR", main="Relationship Between Bird Species' Body Mass and DR")
abline(Model)
summary(Model)
plot(ProjectData$BodySize, ProjectData$HabitatBreadth, pch=16)
FinalProjectPhylogeny <- read.tree("https://dryad-assetstore-merritt-west.s3.us-west-2.amazonaws.com/ark%3A/13030/m5547t8g%7C1%7Cproducer/SummaryTreeEricson.tre?response-content-type=text%2Fplain&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEK7%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCXVzLXdlc3QtMiJGMEQCICtF2nr4cRGBzdJm%2F55wUYNbNTOPZNzagMDPtQKuqDSNAiALEgaJi8O9XE3t3ez%2FnlizIo5Q1Fr9sjBSJy6B5jpEjyr6Awg3EAAaDDQ1MTgyNjkxNDE1NyIMPXkQNDHY8VzOUSFQKtcDPVRIr%2FhbuAcSc8K6%2FYqCr%2BfXdN4wsDm825fkGFezooa5DzomX2izmPgKkXW1jNBmzONrS4C6igSBUaxS%2BtItp8phNSZc9y4vguWYjOWZMD%2BTivOfUl41Zr3SK3V%2B%2B%2FzMTZWxzeyPcyYEM%2BaNHSoFxHkxv6%2FuQ%2FKyGzS2KEaqM01qpHcitMFf0JP4vXelT2FJmGPwxibt0OnCKJnn1guCXHOs%2Fi2O2Yn9qnAS9kFMIRwbyhtfF6KCY1RMtsnIyHMx4FK8R9csgvQMNxwHIZ%2FSFY5Js6%2Bnm29kiHbf8zSTjBUBTTZaWH3oz5NCuSu0Wd6VvIvICArdCJty2CYpXSZC6LqYhb7%2BceGKf%2FIFqGk4WNb1unp06pABCmdtUhbvl9Ov7%2FpA%2F23m2Yhu2oHAoNETXB6i%2F7FTROFO2%2Fh62HXr5jxsOkjyjP%2B6kbp8CdZswdWscHQx4gE8mqs2s24q064uueR3VfuZdiz00urxsKDuqcJ2jNx3qgAWIbr2q5pUZo8ku5%2FJGHo0zzgJugLEB50kCyFOeA0Aw%2BGdEo8mbHJwGu3TrD8QhT%2F7PrwMny6bDFJbT9t7COTkeybareCBJx0ylh2Lp1ZBu7BHkPbfz%2FVOZZ%2BV8%2B0d%2B5giMPaN6ZEGOqYBB6xHf3Sv9oFpS2vE6ToFVSpcrRjYTVq96Ku90rpkkV7pwgDLRHNHEHmwvuKsmnzIQF2FgCJfQrqraKW%2BmS0ifk4WVweeey9%2BMfKnXikXYFqT6WJ%2FgRqCgKzxLc8wXnjkuUJivvgv1f3yptmK%2FU6g%2B1k4TmZLg5eMOjSF7z%2BnFPn4Rq8fCZgHhWFVSoaz0xznHF40Edo0VtshE1Jb31tMwSYXtEIGaQ%3D%3D&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20220322T230212Z&X-Amz-SignedHeaders=host&X-Amz-Expires=14400&X-Amz-Credential=ASIAWSMX3SNWVOL64RWS%2F20220322%2Fus-west-2%2Fs3%2Faws4_request&X-Amz-Signature=1fe26fd5bebf7d4ff989d9480aff29b9b166ebad54edc1a493ad8be0cd8e9795")

plot(FinalProjectPhylogeny, cex=0.25)

tree <- FinalProjectPhylogeny

library(BAMMtools)

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

plot(EData, breaksmethod="jenks")
##NEED TO ADD LEGEND TO INDICATE WHICH COLORS ARE HIGHER DR. 
##ARE BETTER COLORS POSSIBLE?
#################

Trait <- ProjectData[,3]
names(Trait) <- ProjectData[,1]
STRAPPoutput <- traitDependentBAMM(EData, Trait, 100)
STRAPPoutput
#UPDATED HYPOTHESIS 24 MAR 2022: There is no correlation between body mass and diversification rate in the bird species recorded.
#NOTE: I still need to edit my graphs to look better and more professional (add titles, add legends/key, change axes titles, etc.). This will be done by the submission deadline in April.