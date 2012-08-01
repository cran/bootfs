extract_feat_pam <-
function(cvp, SUBDIR) {     
		featmat <- extract_features(cvp)
		featmat_aligned <- align_features(cvp)
		write.csv(featmat, file=paste(SUBDIR, "featurematrix.csv", sep="/"))
		write.csv(featmat_aligned, file=paste(SUBDIR, "featurematrix_aligned.csv", sep="/"))
		invisible(list(featmat=featmat, featmat_aligned=featmat_aligned))
	}
