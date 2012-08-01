\name{resultBS}
\alias{resultBS}
\title{
Summarise the results of a bootstrapping analysis.
}
\description{
Get frequencies of features and the importance graph by combining all bootstrapping runs and classification methods.
}
\usage{
resultBS(results, DIR=NULL, vlabel.cex = 3, filter = 10, 
		useresults=1:length(results), plot=TRUE)
}
\arguments{
  \item{results}{
	Result object from \code{\link{doBS}}. 
}
  \item{DIR}{
	Output base directory. If not NULL, output is stored in this folder. Creates the following files: \code{importance_graph_combined.pdf}, \code{adj_sorted.csv} and \code{tophits.csv}.
}
  \item{vlabel.cex}{
	Node label expansion factor.
}
  \item{filter}{
	Edge occurrence filter. Only show edges with larger occurrence count
	than \code{filter}.
}
  \item{useresults}{
    Vector of integers. Specify the elements of the results list to use for summarisation. Defaults to all (i.e. use results from all feature selection algorithms).
}
  \item{plot}{
	Boolean. If TRUE, show the importance graph plot.
}
}
\details{
Derives the ranking of features across all bootstrapping algorithms,
according to how often they occur in all bootstrapping runs. \code{resultBS} is a list with one element for each classification method. In each of these elements, for every separate classification task (e.g. different outcome groups) one sub list is stored. \code{resultBS} extracts each individual classification task and makes the importance graph and adjacency matrix as described below.\cr

For each classification task, the intersection of the selected features across all algorithms in each bootstrapping iteration is created. Occurences of the intersected features are counted
to create the final ranking.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
A list with one element for each classification task (i.e. group classification). Each element holds the following sub elements:\cr
\item{allsignatures }{Intersection feature sets for each bootstrap run.}
\item{allprots }{Union of all intersection features.}
\item{adj}{Adjacency matrix holding the occurence counts.}
\item{tophits}{Top selected features, ranked according to there occurrence.}
}
\author{
Christian Bender
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

params <- control_params(bstr=5, maxiter=100, maxevals=50, maxRuns=30)
	
## run the bootstrapping
retBS <- doBS(logX, groupings, 
	fs.methods=c("pamr","scad","rf_boruta"),
	DIR=NULL, 
	params=params)

## create the importance graph for all methods
## and export the adjacency matrix containing the 
## numbers of occuerrences of the features, as well 
## as the top hits.
res <- resultBS(retBS, DIR=NULL, vlabel.cex = 3, filter = 0, saveres = FALSE)

## remove the created directory
system("rm -rf bs")

}
}
