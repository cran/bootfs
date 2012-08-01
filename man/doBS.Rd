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
	fs.methods=c("pamr","scad","rf_boruta"),
	DIR="bs", 
	seed=123, bstr=100, saveres=TRUE, jitter=FALSE,
	maxiter=1000, maxevals=500, bounds=NULL,
	max_allowed_feat=NULL, n.threshold=50,
	maxRuns=300)
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
	A character vector naming the algorithms to be used. Currently, the following three algorithms are included: \code{pamr}, \code{scad}, \code{rf_boruta}. Any combination of these three can be used.
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
}
\details{
	Use this function to create the final feature ranking by running
	bootstrapped classification and feature selection with all methods specified in \code{fs.methods}.
}
\value{
	Output is stored in directory \code{DIR}.
}
\references{
Todo.
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

# library(bootFS)
set.seed(1234)
data <- simDataSet(nsam=30, ngen=100, sigma=2, plot=TRUE)
logX <- data$logX
groupings <- data$groupings

## run the bootstrapping
retBS <- doBS(logX, groupings, 
	fs.methods=c("pamr","scad","rf_boruta"),
	DIR="bs", 
	seed=123, bstr=15, saveres=FALSE, jitter=FALSE,
	maxiter=100, maxevals=50, bounds=NULL,
	max_allowed_feat=NULL, n.threshold=50,
	maxRuns=30)
retBS <- doBS(logX, groupings, fs.methods=c("pamr","scad","rf_boruta"),	DIR="bs", seed=123, bstr=15, saveres=FALSE, jitter=FALSE, maxiter=100, maxevals=50, bounds=NULL, max_allowed_feat=NULL, n.threshold=50,	maxRuns=30)

bsres <- makeIG(retBS[[1]], SUBDIR=NULL)

## create the combined importance graph for all methods
## and export the adjacency matrix containing the 
## numbers of occuerrences of the features, as well 
## as the top hits.
res <- resultBS(retBS, DIR="bs", vlabel.cex = 3, filter = 0, saveres = FALSE)

## plot the importance graph directly
ig <- importance_igraph(res$adj, main = "my test", 
        highlight = NULL,	layout="layout.ellipsis",
		pdf=NULL, pointsize=12, tk=FALSE,
		node.color="grey", node.filter=NULL,
		vlabel.cex=1.2, vlabel.cex.min=0.5, vlabel.cex.max=4,
		max_node_cex=8,
        edge.width=1, filter=1, max_edge_cex=2, ewprop=3 )

## remove the created directory
system("rm -rf bs")

}
}
