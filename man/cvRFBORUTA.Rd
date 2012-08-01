\name{cvRFBORUTA}
\alias{cvRFBORUTA}

\title{
Crossvalidation for Random Forests with Boruta feature selection.
}
\description{
Main Wrapper for the Random Forest/Boruta crossvalidation.
}
\usage{
cvRFBORUTA(logX, groupings, DIR, params = NULL)
				%#list(seed = 123, ncv = 5, repeats = 10, 
				%#	maxRuns = 300,
				%#	rfimportance="MeanDecreaseGini",
				%#	ntree=1000,
				%#	localImp=TRUE,
				%#	fs.method="rf_boruta",
				%#	saveres=FALSE))
}
\arguments{
  \item{logX}{
	The data matrix. Samples in rows, features in columns.
}
  \item{groupings}{
	A list of group vectors. Each list element is a named vector (length equals the
	number of samples), holding group assignments for each sample (either
	1 for group A and -1 for group B). 
}
  \item{DIR}{
	The output base directory.
}
  \item{params}{
	A parameter list with the following elements. \code{seed}: a random seed. \code{ncv}: the number of folds in the crossvalidation. \code{repeats}: number of repeats of the CV. \code{maxRuns}: Boruta parameter. \code{rfimportance} Which importance measure to use in the random forest feature ranking. \code{ntree} Number of trees in the random forest. \code{localImp} Local importance in random forest. \code{fs.method} Either \code{rf} or \code{rf_boruta}. \code{saveres} Boolean, if TRUE, save results.
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
Miron B. Kursa, Witold R. Rudnicki (2010). Feature Selection with the Boruta Package. Journal of Statistical Software, 36(11), p. 1-13. URL: http://www.jstatsoft.org/v36/i11/

Breiman, L. (2001), _Random Forests_, Machine Learning 45(1), 5-32.

Breiman, L (2002), ``Manual On Setting Up, Using, And Understanding Random Forests V3.1'', <URL:http://oz.berkeley.edu/users/breiman/Using_random_forests_V3.1.pdf>.

}
\author{
Christian Bender
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
