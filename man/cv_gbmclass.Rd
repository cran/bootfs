\name{cv_gbmclass}
\alias{cv_gbmclass}

\title{
Internal crossvalidation method for GBM classification
}
\description{
Internal crossvalidation method for GBM classification.
}
\usage{
cv_gbmclass(X, logX, ncv = 5, repeats = 10, seed = 123, params,
			avg = "none", spread.estimate = "none",
			colorize = FALSE, minclassN = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
	Grouping info.
}
  \item{logX}{
	The data matrix.
}
  \item{ncv}{
	Number of crossvalidation folds.
}
  \item{repeats}{
	Number of repeats to perform.
}
  \item{seed}{
	A random seed.
}
  \item{params}{
	A parameter list.
}
%  \item{maxRuns}{
%	DEPRECATED.
%}
  \item{avg}{
	averaging in roc curve. See ROCR package.
}
  \item{spread.estimate}{
	spread.estimate. See ROCR package.
}
  \item{colorize}{
	Color the roc curves. See ROCR package.
}
  \item{minclassN}{
	Special variable to treat very small sample sizes. Possibly removed soon.
}
  \item{\dots}{
	Further arguments.
}
}
\details{
	Internal crossvalidation caller.
}
\value{
The cv result.
}

\author{
Christian Bender
}

\seealso{
\code{\link{doCV}}
}
\examples{
\dontrun{
##TODO
}
}
