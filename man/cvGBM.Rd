\name{cvGBM}
\alias{cvGBM}
\title{
Make a crossvalidation using GBM.
}
\description{
Make a crossvalidation using GBM.
}
\usage{
cvGBM(logX, groupings, DIR, params = list(seed = 123, ncv = 5,
		repeats = 10, ntree = 1000, shrinkage = 0.01,
		interaction.depth = 3, bag.fraction = 0.75,
		train.fraction = 0.75, n.minobsinnode = 3,
		n.cores = 1, verbose = TRUE, jitter = FALSE))
}
\arguments{
  \item{logX}{
	The data matrix.
}
  \item{groupings}{
    The list containing the group factors.
}
  \item{DIR}{
	An output directory.
}
  \item{params}{
	A parameter list.
}
}
\details{
	Internal function.
}
\value{
CV result.
}
\references{
J.H. Friedman (2001). "Greedy Function Approximation: A Gradient Boosting Machine," _Annals of Statistics_ 29(5):1189-1232.

J.H. Friedman (2002). "Stochastic Gradient Boosting," _Computational Statistics and Data Analysis_ 38(4):367-378.

}
\author{
Christian Bender
}

\seealso{
\code{\link{doCV}}
}
\examples{
\dontrun{
## TODO
\
}
}
