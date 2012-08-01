\name{bsRFBORUTA}
\alias{bsRFBORUTA}
\title{
Perform RFBORUTA bootstrapping.
}
\description{
Use RFBORUTA algorithm and bootstrapping to derive a robust feature set.
}
\usage{
bsRFBORUTA(logX, groupings, DIR=NULL, params = NULL)
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
	A parameter list with the following elements:
	\code{seed}: A random seed.
	\code{bstr}: Number of bootstrap iterations.
	\code{jitter}: Boolean. Use some additional noise for constant values.
	\code{maxRuns}: Parameter for the Boruta function.
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
