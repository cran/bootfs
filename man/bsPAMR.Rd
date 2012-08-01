\name{bsPAMR}
\alias{bsPAMR}
\title{
Perform PAMR bootstrapping.
}
\description{
Use PAMR algorithm and bootstrapping to derive a robust feature set.
}
\usage{
bsPAMR(logX, groupings, DIR, params = NULL)
}
\arguments{
  \item{logX}{
	The data matrix.
}
  \item{groupings}{
	The grouping variable.
}
  \item{DIR}{
	The output base directory.
}
  \item{params}{
	A parameter list with the following elements:\cr
	\code{seed}: A random seed.\cr
	\code{bstr}: Number of bootstrap iterations.
	\code{jitter}: Boolean. Use some additional noise for constant values.
	\code{ncv}: Number of inner crossvalidation folds.
	\code{max_allowed_feat}: Maximum number of allowed features in the
	feature set.
	\code{n.threshold}: Threshold parameter for PAMR.
	\code{saveres}: Boolean. Stored results of bootstrapping in an R-data
	object.
}
}
\details{
Usually called from \code{\link{doBS}}.
}
\value{
The result list of the bootstrapping.
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}
\seealso{
\code{\link{doBS}}
}
\examples{
\dontrun{
#todo
}
}
