#I collaborated with Rebecca LaRochelle for parts of this assignment.
setwd("~/Desktop/Evolution/Tasks/Task_08")
library(phytools)
library(ape)
text.string <- "(((((((cow, pig), whale), (bat, (lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
#QUESTION 1: Humans are more closely related to goldfish than sharks.
vert.tree
#QUESTION 2: There are no branch lengths in this tree.
str(vert.tree)
tree <- read.tree(text="(((A,B), (C,D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))
tipEdges <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
tree
#QUESTION 3:
plot.phylo(AnolisTree, show.tip.label = FALSE, cex=0.25)
#QUESTION 4:
plot.phylo(AnolisTree, type="radial", cex=0.25)
#QUESTION 5:
plot.phylo(AnolisTree, tip.color="red", cex=0.25)
#QUESTION 6-8:
names(Lengths)[which(Lengths == min(Lengths))]
#Anolis occultus has the shortest edge length among living, nameed species.
AnolisTree2 <- drop.tip(AnolisTree, "Anolis_occultus")
plot.phylo(AnolisTree2, cex=0.25)
pdf(Question10, height=4, width=4)
Question10 <- ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
dev.off()
#The line starts by having a step-like pattern, but these steps get closer together to the point where they don't look like steps anymore as time goes on. The slope of the line changes too. 
#The line never goes down because there can't be 'negative' evolution. Lineages that happened throughout history can't just disappear.
#The slope on the actual data does change. Generally, its steeper at the beginning and gets flatter later on.
#The slope of this curve tells you about the diversification rate. As the slope changes, diversification rate of the clade is changing through time. The fit line shows the average diversification rate throughout the clade's history, and observing the slope at any point on the actual data line will give you the diversification rate at that specific point in time.
#QUESTION 10:
fit.bd(AnolisTree, rho=0.2)
#ML(b/lambda) = 0.8031 (rate new species form)
#ML(d/mu) = 0 (rate species disappear)
#There was no plot generated in QUESTION 10, so I saved and submitted the lineage-through-time plot created in the previous question.
##EXTRA CREDIT
install.packages("rotl")
library(rotl)
FlyStudies <- studies_find_trees(property="ot:ottTaxonName", value="Drosophila", detailed=FALSE)
get_study_tree(FlyStudies)
tree <- get_study_tree(study_id="pg_1144", tree_id="tree2324")
fit.bd(tree, force.ultrametric())
#Received following error message:
#Error in fit.bd(tree) : 
#tree fails is.ultrametric.
#If you believe your tree actually is ultrametric use force.ultrametric & try again.
#fit.bd(tree, force.ultrametric())
#^^Received same error message after attempting to use force.ultrametric