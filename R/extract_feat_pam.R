extract_feat_pam <-
function(cvp, SUBDIR=NULL) {     
		featmat <- extract_features(cvp)
		featmat_aligned <- align_features(cvp)
		if(!is.null(SUBDIR)) {
			write.csv(featmat, file=paste(SUBDIR, "featurematrix.csv", sep="/"))
			write.csv(featmat_aligned, file=paste(SUBDIR, "featurematrix_aligned.csv", sep="/"))
		}
		invisible(list(featmat=featmat, featmat_aligned=featmat_aligned))
	}
