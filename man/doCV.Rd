\name{doCV}
\alias{doCV}

\title{
Performance evaluation by crossvalidation for multiple classification algorithms.
}
\description{
Evaluate the performance of multiple classification/feature selection algorithms using crossvalidation.
}
\usage{
doCV(logX, groupings, fs.methods = c("pamr", "scad", "rf_boruta"), DIR = "cv", seed = 123, ncv = 5, repeats = 10, jitter=FALSE, maxiter = 1000, maxevals = 500, max_allowed_feat = 500, n.threshold = 50, maxRuns = 300)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{ncv}{
	Number of crossvalidation folds.
}
  \item{repeats}{
	Number of crossvalidation repeats.
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
	Use this function to evaluate the performance of classifying the groups assigned in \code{groupings} using classification algorithms defined in \code{fs.methods}. Performance is estimated in a crossvalidation with \code{ncv} folds and \code{repeats} repeats by generating ROC curves showing expected sensitivity and 1-specificity, as well as AUC.
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

## removethe created directory			
system("rm -rf cv")
		
}

}
