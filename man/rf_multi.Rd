\name{rf_multi}
\alias{rf_multi}

\title{
Wrapper for RF Boruta bootstrapping.
}
\description{
Wrapper for RF Boruta bootstrapping.
}
\usage{
rf_multi(X, datX, maxRuns = 500, seed = 123, bstr = 100,
		ntree=500, localImp=TRUE, rfimportance="MeanDecreaseGini",
		fs.method="rf_boruta")
}
\arguments{
  \item{X}{
	List of grouping factors.
}
  \item{datX}{
	Data matrix.
}
  \item{maxRuns}{
	Boruta argument.
}
  \item{seed}{
	Random seed.
}
  \item{bstr}{
	Number of bootstrap iterations.
}
  \item{ntree}{
	Number of trees for randomForest call.
}
  \item{localImp}{
	LocalImportance in randomForest call.
}
  \item{rfimportance}{
	Which measure to use for variable selection in the randomForest call. Either \code{MeanDecreaseGini} or \code{MeanDecreaseAccuracy}.
}
  \item{fs.method}{
	Which method is used, either \code{rf_boruta} for Boruta selection or \code{rf} for default randomForest.
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
