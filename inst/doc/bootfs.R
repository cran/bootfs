### R code from vignette source 'bootfs.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: no.nonsense
###################################################
rm(list=ls())


###################################################
### code chunk number 2: Loadpackage (eval = FALSE)
###################################################
## library(bootfs)


###################################################
### code chunk number 3: SimulateNetwork (eval = FALSE)
###################################################
## set.seed(1234)
## data <- simDataSet(nsam=30, ngen=100, sigma=1.5, plot=TRUE)
## logX <- data$logX
## groupings <- data$groupings


###################################################
### code chunk number 4: runCV (eval = FALSE)
###################################################
## ## run the crossvalidation
## ## note the number of repeats should be set to 10 or so, 
## ## it is set to 2 here to have a low running time of this illustration
## retCV <- doCV(logX, groupings, fs.methods = c("pamr", "scad", "rf_boruta"), 
## 	DIR = "cv", seed = 123, ncv = 5, repeats = 2, 
## 	jitter=FALSE, maxiter = 100, maxevals = 50, 
## 	max_allowed_feat = 50, n.threshold = 50, maxRuns = 30)


###################################################
### code chunk number 5: runBS (eval = FALSE)
###################################################
## ## run the bootstrapping
## retBS <- doBS(logX, groupings, 
## 	fs.methods=c("pamr","scad","rf_boruta"),
## 	DIR="bs", 
## 	seed=123, bstr=15, saveres=FALSE, jitter=FALSE,
## 	maxiter=100, maxevals=50, bounds=NULL,
## 	max_allowed_feat=NULL, n.threshold=50,
## 	maxRuns=30)


###################################################
### code chunk number 6: showSingleImpgraph (eval = FALSE)
###################################################
## ## show an importance ranking for a single 
## ## classification method
## bsres <- makeIG(retBS[[1]], SUBDIR=NULL, prob=.9)


###################################################
### code chunk number 7: makeCombinedImpgraph (eval = FALSE)
###################################################
## ## create the combined importance graph for all methods
## ## and export the adjacency matrix containing the 
## ## numbers of occuerrences of the features, as well 
## ## as the top hits.
## res <- resultBS(retBS, DIR="bs", vlabel.cex = 3, filter = 8, saveres = FALSE)


###################################################
### code chunk number 8: customisedImpGraph (eval = FALSE)
###################################################
## ## plot the importance graph directly. Gives more 
## ## flexibility to adjust the graph
## 
## ig <- importance_igraph(res$adj, main = "my test", 
##         highlight = NULL,	layout="layout.ellipsis",
## 		pdf=NULL, pointsize=12, tk=FALSE,
## 		node.color="grey", node.filter=NULL,
## 		vlabel.cex=2, vlabel.cex.min=0.5, vlabel.cex.max=4,
## 		max_node_cex=8,
##         edge.width=2, filter=8, max_edge_cex=4, ewprop=8 )


###################################################
### code chunk number 9: bootfs.Rnw:214-215
###################################################
toLatex(sessionInfo())


