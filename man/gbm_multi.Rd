\name{gbm_multi}
\alias{gbm_multi}

\title{
Calling function to GBM bootstrapping
}
\description{
Calling function to GBM bootstrapping. Used internally.
}
\usage{
gbm_multi(X, datX, seed = 123, bstr = 100, params = NULL)
}

\arguments{
  \item{X}{
	The grouping factor. A list with one element holding the grouping factor.
}
  \item{datX}{
	The data matrix.
}
  \item{seed}{
	A random seed.
}
  \item{bstr}{
	Integer. Number of bootstrapping iterations.
}
  \item{params}{
	List. A parameter list. See \code{\link{doBS}} for details.
}
}
\details{
	Internally called function to call the bootstrapping method using GBMs.
}
\value{
	List with two elements: \code{fit} holding the bootstrap fits and \code{selprobes} holding the selected features for each bootstrapping run.
}
\references{
	Friedman, J. H.: Stochastic Gradient Boosting. (March 1999)
	Friedman, J. H.: Greedy function approximation: A gradient boosting machine. Ann. Statist. Volume 29, Number 5 (2001), 1189-1232.
}
\author{
Christian Bender
}


\seealso{
\code{\link{doBS}}
\code{\link{doCV}}
}
\examples{
\dontrun{
	## todo
}
}
