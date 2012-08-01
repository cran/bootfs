\name{bsGBM}
\alias{bsGBM}

\title{
Wrapper for GBM bootstrapping
}
\description{
Wrapper for GBM bootstrapping. Called from \code{\link{doBS}}.
}
\usage{
bsGBM(logX, groupings, DIR=NULL, params = NULL)
}

\arguments{
  \item{logX}{
	The data matrix.
}
  \item{groupings}{
	The grouping variables.
}
  \item{DIR}{
	String. Path to output directory.
}
  \item{params}{
	List. Parameter list. See \code{\link{doBS}} for details.
}
}
\details{
	Wrapper function for GBM bootstrapping call.
}
\value{
	List holding the bootstrap results for each grouping variable in \code{grouping}.\cr
	This is a list with one element for each of the \code{bstr} bootstrap iterations. Each element is again a list, which is retrieved by function \code{\link{gbm_multi}}.
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
