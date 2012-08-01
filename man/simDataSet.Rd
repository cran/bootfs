\name{simDataSet}
\alias{simDataSet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
simDataSet - simulation of exemplary dataset
}
\description{
A very simple interface to simulate a dataset containing ngen genes and nsam samples. Two groups are defined, drawn from normal distributions with different parameters.
}
\usage{
simDataSet(nsam = 30, ngen = 100, mu1a = 1.2, mu1b = -0.5, mu2a = -1.2, mu2b = -1.4, sigma = 1, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nsam}{
	Integer. Number of samples.
}
  \item{ngen}{
	Integer. Number of genes.
}
  \item{mu1a}{
	Double. Mean value of first subgroup of genes in the first sample group.
}
  \item{mu1b}{
	Double. Mean value of second subgroup of genes in the first sample group.
}
  \item{mu2a}{
	Double. Mean value of first group of genes in the second sample group.
}
  \item{mu2b}{
	Double. Mean value of second group of genes in the second sample group.
}
  \item{sigma}{
	Positive double. Common standard deviation for the informative genes.
}
  \item{plot}{
	Boolean. Show a heatmap of the sampled data.
}
}
\details{
	Defines two sample groups to be classified. One third of the genes One third of the genes contain the information to classify sample group 1, another third the information to classify sample group2. In each gene group, two subgroups with differing intensity profiles are defined, to get complementary subgroups which in total define the respective sample group.
}
\value{
	A list with two elements:
	\item{logX}{Log intensity values, samples in rows, features in columns.}
	\item{groupings}{List containing one element named grx, which hold the sample group assignment}
}

\author{
Christian Bender.
}


\examples{
	\dontrun{
		my.seed <- 1234
		data <- simDataSet(ngen=100, nsam=30, plot=TRUE)
		
		
		

		## alternative way to sample data	
		my.seed <- runif(n=1, min=1, max=99999999)
		nsam <- 30 ## number of samples
		ngen <- 100 ## number of features
		nsig <- floor(ngen * .33)

		## use simdata from penalizedSVM package
		# 4. add 6 blocks of 5 genes each and only one significant gene 
		# per block. all genes in the block are correlated with constant
		# correlation factor corr.factor=0.8 		
		#train <- sim.data(n = nsam, ng = ngen, nsg = nsig, corr=TRUE, corr.factor=0.8, blocks=TRUE, n.blocks=6, nsg.block=1, ng.block=5, seed=my.seed )

		train <- sim.data(n = nsam, ng = ngen, nsg = nsig, corr=FALSE,  seed=my.seed, p.n.ratio=0.8) 

		logX <- t(train$x)
		groupings <- list(grx=train$y)

		drawheat(logX, groups=groupings[[1]])

	}
}
