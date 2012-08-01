align_features <-
function(cvp) {
		allfeats <- sapply(cvp, get_pam_features)
		allprobes <- sort(unique(unlist(sapply(allfeats, function(x) x[-1]))))
		mat <- matrix("", nrow=length(allprobes)+1, ncol=length(allfeats), dimnames=list(c("00_minerr_percent",allprobes), names(allfeats)))
		for(i in 1:length(allfeats)) {
			ind <- match(allfeats[[i]][-1], allprobes)+1
			mat[ind, i] <- allfeats[[i]][-1]
			mat[1, i] <- allfeats[[i]][1]
		}
		mat
	}
