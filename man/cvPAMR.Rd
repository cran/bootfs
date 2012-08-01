\name{cvPAMR}
\alias{cvPAMR}
\title{
Main wrapper to call PAMR crossvalidation.
}
\description{
Main wrapper to call PAMR crossvalidation.
}
\usage{
cvPAMR(logX, groupings, DIR, params = 
	list(seed = 123, ncv = 5, repeats = 10,
	max_allowed_feat = 500, n.threshold = 50))
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
  \item{DIR}{
	The output base directory.
}
  \item{params}{
	A parameter list with the following elements. \code{seed}: a random seed. \code{ncv}: the number of folds in the crossvalidation. \code{repeats}: number of repeats of the CV. \code{max_allowed_feat}: maximum number of returned features. \code{n.threshold}: PAMR threshold parameter.
}
}
\details{
	Do a \code{ncv}-fold crossvalidation (CV) with \code{repeats} repeats. Each grouping assignment from \code{groupings} will be used as classification task. If more than one grouping is present, the package uses parallelisation in the package \code{multicore}.\cr

	This function is usually called by \code{\link{doCV}}.
}
\value{
	CV result. A list with three elements \code{res, featlist and performance}, holding the crossvalidation data, an extracted features list in each cv-iteration and an overall performance object, holding information on the performance (AUC values and a roc curve object that can be plotted). See \code{\link{resultCV}} for making a summary plot for the CV.
}
\references{
Robert Tibshirani, Trevor Hastie, Balasubramanian Narasimhan, and Gilbert Chu. "Diagnosis of multiple cancer types by shrunken centroids of gene expression" PNAS 2002 99:6567-6572 (May 14)
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}
\seealso{
\code{\link{doCV}}
Help of PAMR package.
}
\examples{
\dontrun{
# todo
}
}
