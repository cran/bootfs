simDataSet <- function(nsam=30, ngen=100, 
	mu1a=1.2, mu1b=-0.5, mu2a=-1.2, mu2b=-1.4, sigma=1, plot=FALSE) {
	
	## get a noise data matrix
	logX <- matrix(rnorm(nsam*ngen, 0, 1), nrow=nsam, 
		dimnames=list(paste("s1", 1:nsam,sep="_"), paste("g",1:ngen,sep=""))) 
		
		
	groupings <- list(grx=c(rep(-1, nsam/2), rep(1,nsam/2)))

	## now add some information so some of the genes
	#igenes1a <- igenes1[1:(length(igenes1)/2)]
	#igenes1b <- igenes1[((length(igenes1)/2)+1):length(igenes1)]
	#igenes2a <- igenes2[1:(length(igenes2)/2)]
	#igenes2b <- igenes2[((length(igenes2)/2)+1):length(igenes2)]

	## define genes holding the grouping information
	igenes1 <- c(1:floor((ngen/3)))
	igenes2 <- c(floor((ngen/3)*2+1):ngen)

	## define sample subgroups, holding the complementary 
	## information on the total grouping
	sg1 <- which(groupings[[1]]==-1)
	sg1a <- sg1[1:(length(sg1)/2)]
	sg1b <- sg1[((length(sg1)/2)+1):length(sg1)]
	sg2 <- which(groupings[[1]]==1)
	sg2a <- sg2[1:(length(sg2)/2)]
	sg2b <- sg2[((length(sg2)/2)+1):length(sg2)]

	logX[sg1a,igenes1] <- rnorm(length(logX[sg1a,igenes1]), mu1a, sigma)
	logX[sg1b,igenes1] <- rnorm(length(logX[sg1b,igenes1]), mu1b, sigma)

	logX[sg2a,igenes2] <- rnorm(length(logX[sg2a,igenes2]), mu2a, sigma)
	logX[sg2b,igenes2] <- rnorm(length(logX[sg2b,igenes2]), mu2b, sigma)

	if(plot) {
		drawheat(logX, groups=groupings[[1]])
	}
	list(logX=logX, groupings=groupings)
}
