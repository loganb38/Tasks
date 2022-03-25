#Final Project Data and Hypothesis
setwd("~/Desktop/Evolution/Tasks/Project")
read.csv("/Users/lcb/Desktop/Evolution/Tasks/Project/Data/finalprojectdata.csv", stringsAsFactors = FALSE)
ProjectData <- read.csv("/Users/lcb/Desktop/Evolution/Tasks/Project/Data/finalprojectdata.csv", stringsAsFactors = FALSE)
head(ProjectData)
#Data able to be read into R, as far as I can tell.
#HYPOTHESIS: There is a positive correlation between body mass and diversification rate in the bird species recorded.
#UPDATED HYPOTHESIS 24 MAR 2022: There is no correlation between body mass and diversification rate in the bird species recorded.
#NOTE: I still need to edit my graphs to look better and more professional (add titles, add legends/key, change axes titles, etc.). This will be done by the submission deadline in April.