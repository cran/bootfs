\name{doCV}
\alias{doCV}

\title{
Performance evaluation by crossvalidation for multiple classification algorithms.
}
\description{
Evaluate the performance of multiple classification/feature selection algorithms using crossvalidation.
}
\usage{
doCV(logX, groupings, fs.methods = c("pamr","gbm","rf"),
		DIR = NULL, seed = 123, ncv = 5, repeats= 10, 
		jitter = FALSE, maxiter = 1000, maxevals = 500,
		max_allowed_feat = 500, n.threshold = 50, maxRuns = 300, 
		params = NULL, saveres = FALSE)
}
\arguments{
  \item{logX}{
	The data matrix. Samples in rows, features in columns.
}
  \item{groupings}{
	A list of group vectors. Each list element is a named vector (length equals the
	number of samples), holding group assignments for each sample (either
	1 for group A and -1 for group B, or multiple labels for multi-class problems). 
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
  \item{params}{
	List. Parameter list for the different methods. If NULL, a default object will be created, including the parameters \code{seed, ncv, repeats, jitter, maxiter, maxevals, max_allowed_feat, n.threshold and maxRuns}, as they are passed to \code{doCV}. Additionally, parameters for the boosting approach are specified. See details for more information.
}
  \item{saveres}{
	Boolean. If TRUE, save results.
}
}
\details{
	Use this function to evaluate the performance of classifying the groups assigned in \code{groupings} using classification algorithms defined in \code{fs.methods}. Performance is estimated in a crossvalidation with \code{ncv} folds and \code{repeats} repeats by generating ROC curves showing expected sensitivity and 1-specificity, as well as AUC.\cr

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
Christian Bender
}
\note{
Todo.
}

\seealso{
R-packages: \code{penalizedSVM}, \code{Boruta}, \code{pamr}, \code{gbm}, \code{randomForest}
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

## run the crossvalidation
## takes a while
retCV <- doCV(logX, groupings, fs.methods = methods, DIR = NULL, params=paramsCV)

}

}
