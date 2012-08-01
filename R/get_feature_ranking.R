get_feature_ranking <-
function(cvret, filename="feature_ranking.txt", write=TRUE) {
		feat <- table(names(unlist(cvret$features)))
		feato <- feat[order(feat,decreasing=TRUE)]
		mat <- cbind(names(feato), feato)
		auctxt <- paste("AUC =",cvret$auc)
		mat <- cbind(mat, c(auctxt, rep("",(nrow(mat)-1))))
		colnames(mat) <- c("protein", "count", "performance")
		if(write) {
			write.csv(mat, file=filename, row.names=FALSE)
		}
		invisible(feato)
	}
