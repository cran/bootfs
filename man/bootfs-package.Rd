\name{bootfs-package}
\alias{bootfs-package}
\alias{bootfs}
\alias{bootFS}
\docType{package}
\title{
	Use multiple feature selection algorithms to derive robust feature sets for two class classification problems.
}
\description{
The package is intended as a convenient wrapper to multiple classification and feature selection algorithms for two class classification problems. For example, to classify high and low risk patients from breast cancer molecular profiling data, classification training, performance evaluation and bootstrapped feature selection is done using multiple algorithms. The combination of the selected feature lists during the bootstrapping yields a highly robust final set of features.
}
\details{
\tabular{ll}{
Package: \tab bootfs\cr
Type: \tab Package\cr
License: \tab GPL (>=2)\cr
}
The following methods are implemented:\cr
SVM + SCAD: Support vector machines with Smoothly clipped absolute deviation feature selection (used from package \code{penalizedSVM}). Also, other models implemented in the penalizedSVM package can be used.\cr
RF + Boruta: Random forests and Boruta feature selection.\cr
RF: Random forest algorithm of Breiman.\cr
PAMR: Prediction analysis for microarrays\cr
GBM: Generalized boosting machine. \cr

Note: DrHSVM method is slow and sometimes throws errors, so use with care.\cr
}
\author{
Christian Bender

Maintainer: Christian Bender <christian.bender@tron-mainz.de>
}
\references{
Boser, B.E., Guyon, I.M., Vapnik, V.N.:  A Training Algorithm for Optimal Margin Classifiers. Proceedings of the 5th Annual ACM Workshop on Computational Learning Theory, 1992.

Zhang, Hao Helen and Ahn, Jeongyoun and Lin, Xiaodong and Park, Cheolwoo: Gene selection using support vector machines with non-convex penalty. Bioinformatics (2006) 22 (1): 88-95

Breiman, L. (2001), Random Forests, Machine Learning 45(1), 5-32

Miron B. Kursa, Witold R. Rudnicki (2010). Feature Selection with the Boruta Package. Journal of Statistical Software, 36(11), p. 1-13. URL: http://www.jstatsoft.org/v36/i11/

Robert Tibshirani, Trevor Hastie, Balasubramanian Narasimhan, and Gilbert Chu. "Diagnosis of multiple cancer types by shrunken centroids of gene expression" PNAS 2002 99:6567-6572 (May 14)

Friedman, J. H.: Stochastic Gradient Boosting. (March 1999)

Friedman, J. H.: Greedy function approximation: A gradient boosting machine. Ann. Statist. Volume 29, Number 5 (2001), 1189-1232.
}
\keyword{ package }
\seealso{
\code{\link{control_params}}, \code{\link{doCV}}, \code{\link{doBS}}, \code{\link{resultBS}}, \code{\link{importance_igraph}}\cr
Help for \code{penalizedSVM}\cr
Help for \code{Boruta}\cr
Help for \code{randomForest}\cr
Help for \code{pamr}\cr
Help for \code{gbm}\cr
}
\examples{
\dontrun{

# library(bootfs)
set.seed(1234)
data <- simDataSet(nsam=30, ngen=100, sigma=2, plot=TRUE)
logX <- data$logX
groupings <- data$groupings

## all methods:
#methods <- c("pamr", "gbm", "rf_boruta", "rf", "scad", "DrHSVM", "1norm", "scad+L2")

## selected methods
methods <- c("pamr", "gbm", "rf")

## create a parameter object used for the different methods
# for crossvalidation
paramsCV <- control_params(seed=123,
				ncv=5, repeats=10, jitter=FALSE,      ## general parameters
				maxiter=100, maxevals=50,             ## svm parameters
				max_allowed_feat=500, n.threshold=50, ## pamr parameters
				maxRuns=300,                          ## RF parameters
				ntree = 1000,                         ## GBM parameters
				shrinkage = 0.01, interaction.depth = 3,
				bag.fraction = 0.75, train.fraction = 0.75, 
				n.minobsinnode = 3, n.cores = 1, 
				verbose = TRUE)
			
# for bootstrapping
paramsBS <- control_params(seed=123,
			jitter=FALSE, bstr=10,                  ## general parameters
			maxiter=100, maxevals=50, bounds=NULL,  ## svm parameters
			max_allowed_feat=500, n.threshold=50,   ## pamr parameters
			maxRuns=300,                            ## RF parameters
			ntree = 1000,                           ## GBM parameters
			shrinkage = 0.01, interaction.depth = 3,
			bag.fraction = 0.75, train.fraction = 0.75, 
			n.minobsinnode = 3, n.cores = 1, 
			verbose = TRUE, saveres=FALSE
)



## run the crossvalidation
## takes a while
retCV <- doCV(logX, groupings, fs.methods = methods, DIR = NULL, params=paramsCV)

# old interface
#retCV <- doCV(logX, groupings, fs.methods = methods, 
#	DIR = "cv", seed = 123, ncv = 5, repeats = 5, 
#	jitter=FALSE, maxiter = 100, maxevals = 50, 
#	max_allowed_feat = 50, n.threshold = 50, maxRuns = 30)

## run the bootstrapping
## takes a while
retBS <- doBS(logX, groupings, fs.methods=methods, DIR=NULL, params=paramsBS)

# old interface
#retBS <- doBS(logX, groupings, 
#	fs.methods=methods,
#	DIR=NULL, 
#	seed=123, bstr=10, saveres=FALSE, jitter=FALSE,
#	maxiter=100, maxevals=50, bounds=NULL,
#	max_allowed_feat=NULL, n.threshold=50,
#	maxRuns=30)

## only use one method
#retBS <- doBS(logX, groupings, fs.methods=methods[1], DIR=NULL, 
#	seed=123, bstr=2, saveres=FALSE, jitter=FALSE,	maxiter=20, 
#	maxevals=20, bounds=NULL,max_allowed_feat=NULL, 
#	n.threshold=25,maxRuns=10)

## create the importance graph using the 3 FS-methods
## and export the adjacency matrix containing the 
## numbers of occurrences of the features, as well 
## as the top hits.
res <- resultBS(retBS, DIR=NULL, vlabel.cex = 3, filter = 0, saveres = FALSE)

## only use methods 1 and 2
res2 <- resultBS(retBS, DIR=NULL, vlabel.cex = 3, filter = 0, saveres = FALSE, 
		useresults=1:2)


## plot the importance graph
ig <- importance_igraph(res$adj, main = "my test", 
        highlight = NULL,	layout="layout.ellipsis",
		pdf=NULL, pointsize=12, tk=FALSE,
		node.color="grey", node.filter=NULL,
		vlabel.cex=1.2, vlabel.cex.min=0.5, vlabel.cex.max=4,
		max_node_cex=8,
        edge.width=1, edge.filter=1, max_edge_cex=2, ewprop=3 )


## show the data and groups
drawheat(logX, groups = groupings[[1]], log = FALSE,
			mar = c(12, 10), distfun = dist.eucsq,
			hclustfun = ward, cexCol = 1, cexRow = 1) 

## subset for the tophits by using logX[tophits,]
adj <- res$adj
ord <- order(diag(res$adj),decreasing=TRUE)
adj <- res$adj[ord,ord]
tophits <- colnames(adj)[1:5]
drawheat(logX[,tophits], groups = groupings[[1]], log = FALSE,
			mar = c(12, 10), distfun = dist.eucsq,
			hclustfun = ward, cexCol = 1, cexRow = 1) 


## do multiclass classification
data(iris)

groupings <- list(Species=iris$Species)
logX <- iris[,1:4]
methods <- c("gbm","rf_boruta","pamr")

## crossvalidation
retCV <- doCV(logX, groupings, fs.methods = methods, DIR = NULL, params=paramsCV)

## bootstrapped feature selection
retBS <- doBS(logX, groupings, fs.methods=methods, DIR=NULL, params=paramsBS)

## make results from all methods used
res <- resultBS(retBS, DIR="bs_multi", vlabel.cex = 3, filter = 0, saveres = FALSE)


## plot the importance graph
ig <- importance_igraph(res$adj, main = "my test", 
        highlight = NULL,	layout="layout.ellipsis",
		pdf=NULL, pointsize=12, tk=FALSE,
		node.color="grey", node.filter=NULL,
		vlabel.cex=1.2, vlabel.cex.min=0.5, vlabel.cex.max=4,
		max_node_cex=8,
        edge.width=1, edge.filter=1, max_edge_cex=2, ewprop=3 )





## removethe created directories			
system("rm -rf cv")
system("rm -rf cv_multi")

## remove the created directory
system("rm -rf bs")
system("rm -rf bs_multi")


}
}
