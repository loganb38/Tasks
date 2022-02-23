#Final Project Data and Hypothesis
setwd("~/Desktop/Evolution/Tasks/Project")
read.csv("/Users/lcb/Desktop/Evolution/finalprojectdata.csv", stringsAsFactors = FALSE)
ProjectData <- read.csv("/Users/lcb/Desktop/Evolution/finalprojectdata.csv", stringsAsFactors = FALSE)
head(ProjectData)
#Data able to be read into R, as far as I can tell.
#HYPOTHESIS: There is a positive correlation between body mass and diversification rate in the bird species recorded.
