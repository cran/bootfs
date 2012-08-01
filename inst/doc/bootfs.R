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
## ## create a parameter object used for the different methods
## # for crossvalidation
## paramsCV <- control_params(seed=123,
##  ncv=5, repeats=2, jitter=FALSE,      ## general parameters
##  maxiter=100, maxevals=50,             ## svm parameters
##  max_allowed_feat=500, n.threshold=50, ## pamr parameters
##  maxRuns=300,                          ## RF parameters
##  ntree = 1000,                         ## GBM parameters
##  shrinkage = 0.01, interaction.depth = 3,
##  bag.fraction = 0.75, train.fraction = 0.75, 
##  n.minobsinnode = 3, n.cores = 1, 
##  verbose = TRUE)
## 
## ## run the crossvalidation
## ## takes a while
## methods <- c("pamr", "scad", "rf_boruta")
## retCV <- doCV(logX, groupings, fs.methods = methods, DIR = NULL, params=paramsCV)


###################################################
### code chunk number 5: runBS (eval = FALSE)
###################################################
## 
## # for bootstrapping
## paramsBS <- control_params(seed=123,
##  jitter=FALSE, bstr=15,                  ## general parameters
##  maxiter=100, maxevals=50, bounds=NULL,  ## svm parameters
##  max_allowed_feat=500, n.threshold=50,   ## pamr parameters
##  maxRuns=300,                            ## RF parameters
##  ntree = 1000,                           ## GBM parameters
##  shrinkage = 0.01, interaction.depth = 3,
##  bag.fraction = 0.75, train.fraction = 0.75, 
##  n.minobsinnode = 3, n.cores = 1, 
##  verbose = TRUE, saveres=FALSE
## )
## ## run the bootstrapping
## ## takes a while
## methods <- c("pamr", "scad", "rf_boruta")
## retBS <- doBS(logX, groupings, fs.methods=methods, DIR="bs", params=paramsBS)
## 


###################################################
### code chunk number 6: showSingleImpgraph (eval = FALSE)
###################################################
## ## show an importance ranking for a single 
## ## classification method
## bsres <- makeIG(retBS[[1]], SUBDIR=NULL, prob=.999)


###################################################
### code chunk number 7: makeCombinedImpgraph (eval = FALSE)
###################################################
## ## create the combined importance graph for all methods
## ## and export the adjacency matrix containing the 
## ## numbers of occuerrences of the features, as well 
## ## as the top hits.
## res <- resultBS(retBS, DIR=NULL, vlabel.cex = 3, filter = 5)


###################################################
### code chunk number 8: customisedImpGraph (eval = FALSE)
###################################################
## ## plot the importance graph directly. Gives more 
## ## flexibility to adjust the graph
## 
## resx <- res[[1]]
## ig <- importance_igraph(resx$adj, main = "my test", 
##  highlight = NULL,	layout="layout.ellipsis",
##  pdf=NULL, pointsize=12, tk=FALSE,
##  node.color="grey", node.filter=NULL,
##  vlabel.cex=2, vlabel.cex.min=0.5, vlabel.cex.max=5,
##  max_node_cex=8,
##  edge.width=2, edge.filter=2, max_edge_cex=5, ewprop=3 )       


###################################################
### code chunk number 9: runMulticlass_loadiris (eval = FALSE)
###################################################
## ## do multiclass classification
## ## load the data
## data(iris)
## 
## groupings <- list(Species=iris$Species)
## logX <- iris[,1:4]
## methods <- c("gbm","rf","pamr")
## 
## paramsCV <- control_params(seed=123,
##  ncv=5, repeats=2, jitter=FALSE,      ## general parameters
##  maxiter=100, maxevals=50,             ## svm parameters
##  max_allowed_feat=500, n.threshold=50, ## pamr parameters
##  maxRuns=300,                          ## RF parameters
##  ntree = 1000,                         ## GBM parameters
##  shrinkage = 0.01, interaction.depth = 3,
##  bag.fraction = 0.75, train.fraction = 0.75, 
##  n.minobsinnode = 3, n.cores = 1, 
##  verbose = TRUE)


###################################################
### code chunk number 10: runMulticlass_cv (eval = FALSE)
###################################################
## ## crossvalidation
## retCV <- doCV(logX, groupings, fs.methods = methods, DIR = NULL, params=paramsCV)


###################################################
### code chunk number 11: runMulticlass_cvresult (eval = FALSE)
###################################################
## resultCV(retCV)


###################################################
### code chunk number 12: runMulticlassBS (eval = FALSE)
###################################################
## 
## paramsBS <- control_params(seed=123,
##  jitter=FALSE, bstr=15,                  ## general parameters
##  maxiter=100, maxevals=50, bounds=NULL,  ## svm parameters
##  max_allowed_feat=500, n.threshold=50,   ## pamr parameters
##  maxRuns=300,                            ## RF parameters
##  ntree = 1000,                           ## GBM parameters
##  shrinkage = 0.01, interaction.depth = 3,
##  bag.fraction = 0.75, train.fraction = 0.75, 
##  n.minobsinnode = 3, n.cores = 1, 
##  verbose = TRUE, saveres=FALSE
## )
## 
## ## bootstrapped feature selection
## retBS <- doBS(logX, groupings, fs.methods=methods, DIR=NULL, params=paramsBS)


###################################################
### code chunk number 13: runMulticlassBS_results (eval = FALSE)
###################################################
## ## make results from all methods used
## res <- resultBS(retBS, DIR=NULL, vlabel.cex = 3, filter = 1)
## 
## 
## ## plot the importance graph
## resx <- res[[1]]
## ig <- importance_igraph(resx$adj, main = "multiclass test, IRIS data", 
##         highlight = NULL,	layout="layout.ellipsis",
## 		pdf=NULL, pointsize=12, tk=FALSE,
## 		node.color="grey", node.filter=NULL,
## 		vlabel.cex=1.2, vlabel.cex.min=0.5, vlabel.cex.max=4,
## 		max_node_cex=8,
##         edge.width=1, edge.filter=1, max_edge_cex=2, ewprop=3 )


###################################################
### code chunk number 14: bootfs.Rnw:340-341
###################################################
toLatex(sessionInfo())


