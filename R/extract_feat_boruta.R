extract_feat_boruta <-
function(res) {
		allfeats <- sapply(res, function(x) {
			if(class(x)=="try-error") {
				NULL
			} else {
				c(x$err,x$selprobes)
			}
		}, simplify=FALSE)
		nrows <- max(sapply(allfeats, length))
		mat <- matrix(NA, nrow=nrows, ncol=length(allfeats), dimnames=list(c("OOB_err_est", paste("feat",(1:(nrows-1)),sep="")), names(allfeats)))
		for(i in 1:length(allfeats)) {
			pad <- rep("", nrows-length(allfeats[[i]]))
			mat[,i] <- c(sort(allfeats[[i]]), pad)
		}
		mat
	}
