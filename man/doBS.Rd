\name{doBS}
\alias{doBS}

\title{
Perform bootstrapped feature selection with multiple algorithms.
}
\description{
Use multiple algorithms for classification and feature selection to
classify samples according to their grouping variables. Use bootstrapped
data sets to take into account the sample variance when deriving the
final feature set.
}
\usage{
doBS(logX, groupings, 
	fs.methods=c("pamr","gbm","rf"),
	DIR=NULL, 
	seed=123, bstr=100, saveres=TRUE, jitter=FALSE,
	maxiter=1000, maxevals=500, bounds=NULL,
	max_allowed_feat=NULL, n.threshold=50,
	maxRuns=300,
	params=NULL)
}
\arguments{
  \item{logX}{
	The data matrix. Samples in rows, features in columns.
}
  \item{groupings}{
	A list. Each list element is a named vector (length equals the
	number of samples), holding group assignments for each sample (either
	1 for group A and -1 for group B). 
}
  \item{fs.methods}{
	A character vector naming the algorithms to be used. Currently, the following three algorithms are included: \code{pamr}, \code{scad}, \code{rf_boruta}, \code{rf}, \code{gbm}. Any combination of these three can be used. Note that multiclass classification is only possible with \code{pamr, rf_boruta, rf, gbm}.
}
  \item{DIR}{
	The output base directory.
}
  \item{seed}{
	A random seed. Is set before each of the applied CV runs, to synchronise sampling of the training and test sets.
}
  \item{bstr}{
	Number of bootstrap runs.
}
  \item{saveres}{
	Save bootstrap results for each feature selection algorithm int \code{DIR}.
}
  \item{jitter}{
	Boolean. Introduce some small noise to the data. Used if many data points
	are constant, as for example in RNASeq data, where many values are
	zero. Note: this might affect the result substantially.
}
  \item{maxiter}{
	Parameter for SCAD SVM from \code{penalizedSVM} package.
}
  \item{maxevals}{
	Parameter for SCAD SVM from \code{penalizedSVM} package.
}
  \item{bounds}{
	Parameter for SCAD SVM from \code{penalizedSVM} package.
}
  \item{max_allowed_feat}{
	Parameter for PAMR features selection. How many features should be maximally returned.
}
  \item{n.threshold}{
	Parameter for PAMR from \code{pamr} package.
}
  \item{maxRuns}{
	Parameter for Random Forest/Boruta from \code{Boruta} package.
}
  \item{params}{
	List. Parameter list for the different methods. Can be created using the function \code{\link{control_params}}. If NULL, a default object will be created with this function. See the help for \code{control_params} and the details for more information.
}
}
\details{
	Use this function to create the final feature ranking by running
	bootstrapped classification and feature selection with all methods specified in \code{fs.methods}.\cr

	The function argument \code{params} is a named list containing the parameters used for the different methods. In general, the arguments for each applied method can be specified. The following parameters can be specified, listed by the method:\cr
	\describe{
	\item{general parameters}{seed=123, ncv=5, repeats=10, jitter=FALSE}
	\item{pamr}{max_allowed_feat=500, n.threshold=50}
	\item{rf_boruta}{maxRuns=300}
	\item{rf}{ntree=500, rfimportance="MeanDecreaseGini", localImp=TRUE}
	\item{svmSCAD (and other svm methods)}{maxiter=1000, maxevals=500}
	\item{gbm}{ntree=1000, shrinkage=0.01,interaction.depth=3,bag.fraction=0.75, train.fraction=0.75, n.minobsinnode=3,n.cores=1,verbose=TRUE}
	}
	It is easiest to generate the control parameter list by using \code{\link{control_params}}.
}
\value{
	Output is stored in directory \code{DIR}.
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
\author{
Christian Bender (christian.bender@tron-mainz.de)
}
\note{
Todo.
}

\seealso{
R-packages: \code{penalizedSVM}, \code{Boruta}, \code{pamr}
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
retCV <- doCV(logX, groupings, fs.methods = methods, DIR = "cv", params=paramsCV)

# old interface
#retCV <- doCV(logX, groupings, fs.methods = methods, 
#	DIR = "cv", seed = 123, ncv = 5, repeats = 5, 
#	jitter=FALSE, maxiter = 100, maxevals = 50, 
#	max_allowed_feat = 50, n.threshold = 50, maxRuns = 30)

## run the bootstrapping
## takes a while
retBS <- doBS(logX, groupings, fs.methods=methods, DIR="bs", params=paramsBS)


## create the importance graph using the 3 FS-methods
## and export the adjacency matrix containing the 
## numbers of occurrences of the features, as well 
## as the top hits.
res <- resultBS(retBS, DIR="bs", vlabel.cex = 3, filter = 0, saveres = FALSE)

## only use methods 1 and 2
res2 <- resultBS(retBS, DIR="bs", vlabel.cex = 3, filter = 0, saveres = FALSE, 
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
retCV <- doCV(logX, groupings, fs.methods = methods, DIR = "cv_multi", params=paramsCV)

## bootstrapped feature selection
retBS <- doBS(logX, groupings, fs.methods=methods, DIR="bs_multi", params=paramsBS)

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
