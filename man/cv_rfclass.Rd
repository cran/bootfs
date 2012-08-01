\name{cv_rfclass}
\alias{cv_rfclass}

\title{
Wrapper for RF Boruta classification crossvalidation.
}
\description{
Wrapper for RF Boruta classification crossvalidation.
}
\usage{
cv_rfclass(X, logX, ncv=5, repeats=10, seed=123,
		maxRuns=500, avg="none", spread.estimate="none",
		colorize=FALSE, minclassN=NULL,
		ntree=1000, localImp=TRUE, rfimportance="MeanDecreaseGini",
		fs.method="rf_boruta", 
		...)
}
\arguments{
  \item{X}{
	A list of group vectors. Each list element is a named vector (length equals the
	number of samples), holding group assignments for each sample (either
	1 for group A and -1 for group B). 
}
  \item{logX}{
  	The data matrix. Samples in rows, features in columns.
}
  \item{ncv}{
	Number of crossvalidation folds.
}
  \item{repeats}{
  	Number of crossvalidation repeats.
}
  \item{seed}{
	A random seed. 
}
  \item{maxRuns}{
	scad parameter.
}
  \item{avg}{
	DEPRECATED. roc curve parameter.
}
  \item{spread.estimate}{
	DEPRECATED. roc curve parameter.
}
  \item{colorize}{
%%     ~~Describe \code{colorize} here~~
}
  \item{minclassN}{
	DEPRECATED. roc curve parameter.
}
  \item{\dots}{
	Additional arguments.
}
}
\details{
An internal function, which is usually not called directly.
}
\value{
Todo.
}
\references{
Todo.
}
\author{
Christian Bender
}

\seealso{
Todo.
}
\examples{
\dontrun{
#Todo.
}
}
\keyword{ internal }
