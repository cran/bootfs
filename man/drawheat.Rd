\name{drawheat}
\alias{drawheat}

\title{
Wrapper for heatmap drawing.
}
\description{
Wrapper for heatmap drawing.
}
\usage{
drawheat(thdat, groups = NULL, log = FALSE,
			mar = c(12, 10), distfun = dist.eucsq,
			hclustfun = ward, cexCol = 1, cexRow = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{thdat}{
	A numeric data matrix. Samples in rows, features in columns.
}
  \item{groups}{
	A character vector with as many elements as \code{thdat} has rows, 
	i.e. samples, assigning a group to each sample.
}
%  \item{sidecols}{
%	A list containing two elements. \itemize{
%		\item{ccols}{A color vector matching the colors for the groups
%		for each sample.}
%		\item{cvec}{A color vector with one color for each group to be
%		displayed above the columns of the heatmap. Must be named with
%		the group names, which then appear in the legend.}
%	}
%}
  \item{log}{
	Boolean. Log the data or not.
}
  \item{mar}{
	The margin argument to \code{heatmap.2} function. Vector of numeric,
	specifying the bottom and left margins, respectively.
}
  \item{distfun}{
	Which distance function to use. Default is \code{\link{dist.eucsq}}.
}
  \item{hclustfun}{
	Which clusterin algorithm. Default is \code{\link{ward}}.
}
  \item{cexCol}{
	Expansion factor for column names.
}
  \item{cexRow}{
	Expansion factor for row names.
}
}
\details{
	Simple heatmap wrapper calling \code{heatmap.2} from the \code{gplots}
	package internally.
}
\value{
Empty.
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}

\seealso{
Help of the \code{gplots} package.
}
\examples{
\dontrun{
	sample1 <- matrix(rnorm(100, 100, 100), ncol=5)
	sample2 <- matrix(rnorm(100, 500, 100), ncol=5)
	thdat <- cbind(sample1, sample2)
	colnames(thdat) <- c(paste("sample1", 1:5, sep="_"), paste("sample2", 1:5, sep="_"))
	rownames(thdat) <- paste("g", 1:nrow(thdat), sep="")
	thdat <- t(thdat)
	groups <- rep(c("sample1", "sample2"), each=5)
	drawheat(thdat, groups = groups, log = FALSE, mar = c(12, 10), distfun = dist.eucsq,
			hclustfun = ward, cexCol = 1, cexRow = 1)	
}
}
