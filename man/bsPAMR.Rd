\name{bsPAMR}
\alias{bsPAMR}
\title{
Perform PAMR bootstrapping.
}
\description{
Use PAMR algorithm and bootstrapping to derive a robust feature set.
}
\usage{
bsPAMR(logX, groupings, DIR=NULL, params = NULL)
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
The result list of the bootstrapping, which is a list: For each classification instance in \code{groupings}, one element is saved, each a list with \code{bstr} elements, from which each again is a list holding the following elements:\cr
\item{histtr}{training object generated with pamr.train.}
\item{histcv}{cv object generated with pamr.cv}
\item{tmin}{Minimal threshold value for the feature selection.}
\item{selected}{Indices of selected features at \code{tmin}.}
\item{selected_names}{Names of selected features at \code{tmin}.}

To get to one bootstrapping element, one could use for example: \code{returnobject[[1]][[1]][["tmin"]]} to get access to the minimal threshold of the first bootstrapping iteration in the first classification problem.
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
