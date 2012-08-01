\name{cvRFBORUTA}
\alias{cvRFBORUTA}

\title{
Crossvalidation for Random Forests with Boruta feature selection.
}
\description{
Main Wrapper for the Random Forest/Boruta crossvalidation.
}
\usage{
cvRFBORUTA(logX, groupings, DIR, params = list(seed = 123, ncv = 5, repeats = 10, maxRuns = 300))
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
	A parameter list with the following elements. \code{seed}: a random seed. \code{ncv}: the number of folds in the crossvalidation. \code{repeats}: number of repeats of the CV. \code{maxRuns}: Boruta parameter.
}
}
\details{
	Do a \code{ncv}-fold crossvalidation (CV) with \code{repeats} repeats. Each grouping assignment from \code{groupings} will be used as classification task. If more than one grouping is present, the package uses parallelisation in the package \code{multicore}.\cr

	This function is usually called by \code{\link{doCV}}.
}
\value{
	None. The output is stored as PDF-file and an RData object into directory \code{DIR}. The PDF holds one ROC curve for each classification from \code{groupings}, showing the expected performance of the classification.
}
\references{
Miron B. Kursa, Witold R. Rudnicki (2010). Feature Selection with the Boruta Package. Journal of Statistical Software, 36(11), p. 1-13. URL: http://www.jstatsoft.org/v36/i11/
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}

\seealso{
\code{\link{doCV}}
Help of Boruta package.
}
\examples{
	\dontrun{
#todo
}
}
