setwd("~/Desktop/Evolution/Tasks/Task_07")
#I collaborated with Rebecca LaRochelle and others for parts of this assignment, mainly through discussion about it.
#I also used the information found at this link (https://github.com/ropensci/paleobioDB/blob/master/README.md) on GitHub for assistance in installing, navigating, and reading the paleobioDB data.
install.packages("paleobioDB")
library(paleobioDB)
setwd("~/Desktop/Evolution/Tasks/Task_07")
canidae <- pbdb_occurrences(limit="all", base_name="canidae", vocab="pbdb", interval="Quaternary", show=c("coords", "phylo", "ident"))
head(canidae)
pbdb_map(canidae)
felidae <- pbdb_occurrences(limit="all", base_name="felidae", vocab="pbdb", interval="Quaternary", show=c("coords", "phylo", "ident"))
head(felidae)
pbdb_map(felidae)
cervidae <- pbdb_occurrences(limit="all", base_name="cervidae", vocab="pbdb", interval="Quaternary", show=c("coords", "phylo", "ident"))
head(cervidae)
pbdb_map(cervidae)
#In the below code, I am attempting to read in data from an excel spreadsheet (.csv) into R.
read.csv("/Users/lcb/Desktop/Evolution/finalprojectdata.csv", stringsAsFactors = FALSE)
FinalProjectRawData <- read.csv("/Users/lcb/Desktop/Evolution/finalprojectdata.csv", stringsAsFactors = FALSE)
FinalProjectData <- data.frame(FinalProjectRawData$Species, FinalProjectRawData$BodySize..avg.body.mass.grams., FinalProjectRawData$DR)
head(FinalProjectData)
mean(FinalProjectData$FinalProjectRawData.BodySize..avg.body.mass.grams.)
mean(FinalProjectData$FinalProjectRawData.DR)
#It appears to be a success!