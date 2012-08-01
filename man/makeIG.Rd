\name{makeIG}
\alias{makeIG}
\title{
Create an importance graph from a bootstrapping result of a single classification method.
}
\description{
After running \code{\link{bsSCAD}, \link{bsPAMR} or \link{bsRFBORUTA}},
the final feature ranking is shown as egg-shaped graph using this
function.
}
\usage{
makeIG(res_bstr, SUBDIR=NULL, prob=.975)
}
\arguments{
  \item{res_bstr}{
	Result list from PAMR, SCAD or RF_Boruta bootstrapping.
}
  \item{SUBDIR}{
	Where to store the output. If NULL, the importance graph is plotted 
	to the standard plotting device.
}
  \item{prob}{
	Quantile in [0,1], defines the cutoff, at which frequency of cooccuring features an edge will be drawn between the two features in the importance graph.
}
}
\details{
	For any bootstrapping feature selection a result list is generated.
	The observed feature sets during all of the bootstrapping runs
	are combined into a graph representing the number of occurrences
	of a feature as nodes, as well as the cooccurrences of pairs of features
	as edges. The width of the nodes is proportional to their frequency of occurrence,
	the width of the edges proportional to the number of cooccurrences
	of two features in all boostrapping feature sets. Edges are drawn for 
	the top 10% of cooccurring feature pairs.
}
\value{
	A list with three elements:
	\item{allsignatures}{All bootstrapping feature sets.}
	\item{allprots}{Union of all features that occur in any feature set.}
	\item{adj}{An adjacency matrix with the counts of (co)occurrences.}
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}

\seealso{
\code{\link{doBS}}
\code{\link{bsSCAD}}
\code{\link{bsPAMR}}
\code{\link{bsRFBORUTA}}
}
\examples{
\dontrun{
# library(bootfs)
set.seed(1234)
data <- simDataSet(nsam=30, ngen=100, sigma=2, plot=TRUE)
logX <- data$logX
groupings <- data$groupings

## run the bootstrapping
retBS <- doBS(logX, groupings, 
	fs.methods=c("pamr","scad","rf_boruta"),
	DIR="bs", 
	seed=123, bstr=5, saveres=FALSE, jitter=FALSE,
	maxiter=100, maxevals=50, bounds=NULL,
	max_allowed_feat=NULL, n.threshold=50,
	maxRuns=30)

## summarize a single bootstrapping run into an importance graph
bsres <- makeIG(retBS[[1]], SUBDIR=NULL, prob=.975)

## remove the created directory
system("rm -rf bs")

}
}
