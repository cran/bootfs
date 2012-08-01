extract_features <-
function(cvp) {
		allfeats <- sapply(cvp, get_pam_features)
		nrows <- max(sapply(allfeats, length))
		mat <- matrix(NA, nrow=nrows, ncol=length(allfeats), dimnames=list(c("minerr_percent", paste("feat",(1:(nrows-1)),sep="")), names(allfeats)))
		for(i in 1:length(allfeats)) {
			pad <- rep("", nrows-length(allfeats[[i]]))
			mat[,i] <- c(sort(allfeats[[i]]), pad)
		}
		mat
	}
