\name{resultBS}
\alias{resultBS}
\title{
Summarise the results of a bootstrapping analysis.
}
\description{
Get frequencies of features and importance graph by cobining all bootstrapping runs and classification methods.
}
\usage{
resultBS(results, DIR=".", vlabel.cex = 3, filter = 10, saveres = TRUE, useresults=1:length(results))
}
\arguments{
  \item{results}{
	Result object from \code{\link{doBS}}.
}
  \item{DIR}{
	Output base directory.
}
  \item{vlabel.cex}{
	Node label expansion factor.
}
  \item{filter}{
	Edge occurrence filter. Only show edges with larger occurrence count
	than \code{filter}.
}
  \item{saveres}{
	Boolean. Should output be written and the importance graph saved in
	directory \code{DIR}. Creates the following files: \code{importance_graph_combined.pdf}, \code{adj_sorted.csv} and \code{tophits.csv}.
}
  \item{useresults}{
    Vector of integers. Specify the elements of the results list to use for summarisation. Defaults to all (i.e. use results from all feature selection algorithms).
}
}
\details{
Derives the ranking of features across all bootstrapping algorithms,
according to how often they occur in all bootstrapping runs. For each
run, the intersection of the selected features across all algorithms
is created. Finally, occurences of the intersected features are counted
to create the final ranking.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{allsignatures }{Intersection feature sets for each bootstrap run.}
\item{allprots }{Union of all intersection features.}
\item{adj}{Adjacency matrix holding the occurence counts.}
\item{tophits}{Top selected features, ranked according to there occurrence.}
}
\author{
Christian Bender (christian.bender@tron-mainz.de)
}
\seealso{
\code{\link{doBS}}
}
\examples{
\dontrun{
# library(bootfs)
set.seed(1234)
data <- simDataSet(nsam=30, ngen=100, sigma=2, plot=TRUE)
logX <- data$logX
groupings <- data$groupings

## run the crossvalidation
retCV <- doCV(logX, groupings, fs.methods = c("pamr", "scad", "rf_boruta"), 
	DIR = "cv", seed = 123, ncv = 5, repeats = 2, 
	jitter=FALSE, maxiter = 100, maxevals = 50, 
	max_allowed_feat = 50, n.threshold = 50, maxRuns = 30)
		
## run the bootstrapping
retBS <- doBS(logX, groupings, 
	fs.methods=c("pamr","scad","rf_boruta"),
	DIR="bs", 
	seed=123, bstr=5, saveres=FALSE, jitter=FALSE,
	maxiter=100, maxevals=50, bounds=NULL,
	max_allowed_feat=NULL, n.threshold=50,
	maxRuns=30)

## create the importance graph for all methods
## and export the adjacency matrix containing the 
## numbers of occuerrences of the features, as well 
## as the top hits.
res <- resultBS(retBS, DIR="bs", vlabel.cex = 3, filter = 0, saveres = FALSE)

## remove the created directory
system("rm -rf bs")

}
}
