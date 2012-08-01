extract_features_rf_boruta <-
function(res,SUBDIR) {
		featmat <- extract_feat_boruta(res)
		featmat_aligned <- align_feat_boruta(res)
		write.csv(featmat, file=paste(SUBDIR, "featurematrix.csv", sep="/"))
		write.csv(featmat_aligned, file=paste(SUBDIR, "featurematrix_aligned.csv", sep="/"))
		invisible(list(featmat=featmat, featmat_aligned=featmat_aligned))
	}
