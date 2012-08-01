\name{bootFS-package}
\alias{bootFS-package}
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
Package: \tab bootFS\cr
Type: \tab Package\cr
%Version: \tab 1.0.3\cr
%Date: \tab 2012-08-01\cr
License: \tab GPL (>=2)\cr
}
The following methods are implemented:\cr
SVM + SCAD: Support vector machines with Smoothly clipped absolute deviation feature selection (used from package \code{penalizedSVM}).\cr
RF + Boruta: Random forests and Boruta feature selection.\cr
PAMR: Prediction analysis for microarrays\cr

}
\author{
Christian Bender

Maintainer: Who to complain to <yourfault@somewhere.net>
Christian Bender <christian.bender@tron-mainz.de>
}
\references{
Zhang, Hao Helen and Ahn, Jeongyoun and Lin, Xiaodong and Park, Cheolwoo: Gene selection using support vector machines with non-convex penalty. Bioinformatics (2006) 22 (1): 88-95

Miron B. Kursa, Witold R. Rudnicki (2010). Feature Selection with the Boruta Package. Journal of Statistical Software, 36(11), p. 1-13. URL: http://www.jstatsoft.org/v36/i11/


Robert Tibshirani, Trevor Hastie, Balasubramanian Narasimhan, and Gilbert Chu. "Diagnosis of multiple cancer types by shrunken centroids of gene expression" PNAS 2002 99:6567-6572 (May 14)
}
\keyword{ package }
\seealso{
\code{\link{doCV}}, \code{\link{doBS}}, \code{\link{resultBS}}, \code{\link{importance_igraph}}\cr
Help for \code{penalizedSVM}\cr
Help for \code{Boruta}\cr
Help for \code{pamr}\cr
}
\examples{
\dontrun{

# library(bootFS)
nsam <- 30 ## number of samples
ngen <- 100 ## number of features

## get a noise data matrix
logX <- matrix(rnorm(nsam*ngen, 0, 10), nrow=nsam, 
	dimnames=list(paste("s1", 1:nsam,sep="_"), paste("g",1:ngen,sep=""))) 
groupings <- list(grx=c(rep(-1, nsam/2), rep(1,nsam/2)))
## now add some information so some of the genes
igenes <- c(1:10)
sg <- c(1,nsam/3,2*nsam/3,nsam)
logX[sg[1]:sg[2],igenes] <- logX[sg[1]:sg[2],igenes] - 5
logX[(sg[2]+1):sg[3],igenes] <- logX[(sg[2]+1):sg[3],igenes] + 10

## run the crossvalidation
retCV <- doCV(logX, groupings, fs.methods = c("pamr", "scad", "rf_boruta"), 
	DIR = "cv", seed = 123, ncv = 5, repeats = 2, 
	jitter=FALSE, maxiter = 100, maxevals = 50, 
	max_allowed_feat = 50, n.threshold = 50, maxRuns = 30)
		
## run the bootstrapping
retBS <- doBS(logX, groupings, 
	fs.methods=c("pamr","scad","rf_boruta"),
	DIR="bs", 
	seed=123, bstr=5, saveres=FALSE, jitter=FALSE,
	maxiter=100, maxevals=50, bounds=NULL,
	max_allowed_feat=NULL, n.threshold=50,
	maxRuns=30)

## create the importance graph for all methods
## and export the adjacency matrix containing the 
## numbers of occuerrences of the features, as well 
## as the top hits.
res <- resultBS(retBS, DIR="bs", vlabel.cex = 3, filter = 0, saveres = FALSE)

## plot the importance graph
ig <- importance_igraph(res$adj, main = "my test", 
        highlight = NULL,	layout="layout.ellipsis",
		pdf=NULL, pointsize=12, tk=FALSE,
		node.color="grey", node.filter=NULL,
		vlabel.cex=1.2, vlabel.cex.min=0.5, vlabel.cex.max=4,
		max_node_cex=8,
        edge.width=1, filter=1, max_edge_cex=2, ewprop=3 )


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



## removethe created directory			
system("rm -rf cv")

## remove the created directory
system("rm -rf bs")


}
}
