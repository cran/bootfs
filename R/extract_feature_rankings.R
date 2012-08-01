extract_feature_rankings <-
function(cvp, allpr, write=TRUE, DIR=".") {
		feats <- list()
		for(i in 1:length(cvp)) {
			if(class(cvp[[i]])=="list") {
				feats[[i]] <- get_feature_ranking(cvp[[i]], filename=paste(DIR, paste("feature_ranking_",names(cvp)[i],".csv",sep=""), sep="/"), write=write)
			} else {
				feats[[i]] <- NULL
			}
		}
		## get the length of the feature vectors
		featsl <- sapply(feats, length) 

		ffmat <- NULL
		for(i in 1:length(feats)) {
			ff <- feats[[i]]
			if(is.null(feats[[i]]))
				next
			pad <- rep(0, length(allpr)-featsl[i])
			if(length(pad)>0) {
				names(pad) <- setdiff(allpr, names(ff))
				ff <- c(ff, pad)
			}
			rn <- names(ff)
			ff <- c(ff, cvp[[i]]$auc)
			names(ff) <- c(rn, "00_AUC_")
			ffmat <- cbind(ffmat, ff[order(names(ff))])
		}
		colnames(ffmat) <- names(cvp)[!sapply(feats, is.null)]
		if(write) {
			write.csv(ffmat, file=paste(DIR, "feature_ranking.csv", sep="/"))
		}
		invisible(ffmat)
	}
