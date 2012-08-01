align_feat_boruta <-
function(res) {
		allfeats <- sapply(res, function(x) {
			if(class(x)=="try-error") {
				NULL
			} else {
				c(x$err,x$selprobes)
			}
		}, simplify=FALSE)
		allprobes <- sort(unique(unlist(sapply(allfeats, function(x) x[-1]))))
		mat <- matrix("", nrow=length(allprobes)+1, ncol=length(allfeats), dimnames=list(c("OOB_err_est",allprobes), names(allfeats)))
		for(i in 1:length(allfeats)) {
			ind <- match(allfeats[[i]][-1], allprobes)+1
			if(length(ind)!=0) {
				mat[ind, i] <- allfeats[[i]][-1]
				mat[1, i] <- allfeats[[i]][1]
			}
		}
		mat
	}
