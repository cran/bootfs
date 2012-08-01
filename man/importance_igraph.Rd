\name{importance_igraph}
\alias{importance_igraph}

\title{
Graphically represent the (co-)occurrences of a set of features, derived 
in a bootstrapped feature selection.
}
\description{
Graphically represent the (co-)occurrences of a set of features which are 
derived from a bootstrapped feature selection with \code{\link{doBS}}. 
Shows how often a feature occurs during classification bootstrapping 
and how often features occur together during the bootstrapping.
}
\usage{
importance_igraph(phi, main = "", 
        highlight = NULL,layout="layout.ellipsis",
		pdf=NULL, pointsize=12, tk=FALSE,
		## provide an intuitive way of filtering
		node.filter=NULL, ## show all nodes by default
		edge.filter=10, ## only edges with more than 10 occurences
		node.color="grey", edge.color="#000000AA",
		vlabel.cex=0.6, vlabel.cex.min=0.5, vlabel.cex.max=4, max_node_cex=8,
        edge.width=1, max_edge_cex=6, ewprop=3)
        
%phi, main = "", 
%        highlight = NULL,	layout="layout.ellipsis",
%		pdf=NULL, pointsize=12, tk=FALSE,
%		node.color="grey", node.filter=NULL,
%		vlabel.cex=0.6, vlabel.cex.min=0.5, vlabel.cex.max=4,
%		max_node_cex=8,
 %       edge.width=1, filter=10, max_edge_cex=6, ewprop=3)
}
\arguments{
  \item{phi}{
    An adjacency matrix. Nonzero values indicate connections between the
    features specified in the column and row names.
}
  \item{main}{
    Character. The title to be plotted.
}
  \item{highlight}{
    Vector of feature names which should be highlighted in red.
}
\item{layout}{
    Which layout to choose. Can be a matrix with the x and y coordinates 
    for each node or a layout function (e.g. layout.circle etc.) or 
    \code{layout.ellipsis}, in which case an egg-shaped graph is 
    drawn.
}
  \item{pdf}{
    Path were to store the graph in pdf format.
}
  \item{pointsize}{
    Pointsize argument for the pdf device function. Used when \code{pdf} 
    is set.
}
  \item{tk}{
    Make tk-plot.
}
  \item{node.color}{
    Color of the nodes.
}
  \item{edge.color}{
    Color of the edges.
}
  \item{node.filter}{
    Only show nodes with frequency greater than or equal \code{node.filter}. Nodes with smaller frequency will be plotted very small.
}
  \item{vlabel.cex}{
    Expansion factor for node labels.
}
  \item{vlabel.cex.min}{
    Minimal node expansion factor. Set to very small value, e.g. 0.01
    to make node labels to disappear nearly completely.
}
  \item{vlabel.cex.max}{
    Maximal node expansion factor. Used to prevent plotting of overly
    sized node labels. A value of 4 seems to be reasonable.
}
  \item{max_node_cex}{
    Maximum node size. 
}
  \item{edge.width}{
    Edge width parameter. Used as basis edge width for the calculation
    of the edge widths when \code{weights} are supplied. Otherwise,
    it determines the edge width for all edges.
}
  \item{edge.filter}{
    Integer. Only show edges with weight larger than \code{filter}.
}
  \item{max_edge_cex}{
    Maximum edge expansion factor. The edge with the highest frequency
    in \code{weights} will be plotted using this factor. All other edge
    widths are scaled appropriately.
}
  \item{ewprop}{
	Edge width proportionality factor. Used when scaling the edge widths
	proportional to the frequency given in \code{weights}. A value of
	1 means that edge width are scaled down with decreasing frequency in
	a linear fashion, a value of 2 means quadratic scaling, 3 means cubic
	scaling etc. Thus, \code{ewprop} controls how fast edges become thinner.
}
}
\details{
This function creates an egg shaped plot of the nodes and their 
occurrences during the bootstrapping feature selection. The larger the 
nodes, the more often they occur. Edges indicate nodes which occur 
together in a feature set. The thicker the edge, the more often the 
two adjacent nodes occur together during the bootstrapping.
}
\value{
The igraph object and layout.
}

\author{
Christian Bender (christian.bender@tron-mainz.de)
}

\examples{
\dontrun{
# library(bootFS)
nsam <- 30 ## number of samples
ngen <- 100 ## number of features

## get a noise data matrix
logX <- matrix(rnorm(nsam*ngen, 0, 10), nrow=nsam, 
	dimnames=list(paste("s1", 1:nsam,sep="_"), paste("g",1:ngen,sep=""))) 
groupings <- list(grx=c(rep(-1, nsam/2), rep(1,nsam/2)))
## now add some information so some of the genes
igenes <- c(1:10)
sg <- c(1,nsam/3,2*nsam/3,nsam)
logX[sg[1]:sg[2],igenes] <- logX[sg[1]:sg[2],igenes] - 5
logX[(sg[2]+1):sg[3],igenes] <- logX[(sg[2]+1):sg[3],igenes] + 10

## run the bootstrapping
retBS <- doBS(logX, groupings, 
	fs.methods=c("pamr","scad","rf_boruta"),
	DIR="bs", 
	seed=123, bstr=5, saveres=FALSE, jitter=FALSE,
	maxiter=100, maxevals=50, bounds=NULL,
	max_allowed_feat=NULL, n.threshold=50,
	maxRuns=30)

bsres <- makeIG(retBS[[1]], SUBDIR=NULL)

## create the combined importance graph for all methods
## and export the adjacency matrix containing the 
## numbers of occuerrences of the features, as well 
## as the top hits.
res <- resultBS(retBS, DIR="bs", vlabel.cex = 3, filter = 0, saveres = FALSE)

## plot the importance graph directly
ig <- importance_igraph(res$adj, main = "my test", 
        highlight = NULL,	layout="layout.ellipsis",
		pdf=NULL, pointsize=12, tk=FALSE,
		node.color="grey", node.filter=NULL,
		vlabel.cex=1.2, vlabel.cex.min=0.5, vlabel.cex.max=4,
		max_node_cex=8,
        edge.width=1, filter=1, max_edge_cex=2, ewprop=3 )

## remove the created directory
system("rm -rf bs")

}
}
