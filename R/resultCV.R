#' create a result plot for all performed crossvalidations
#'
#' Use the ouptu of a call to \code{doCV} and create a summary of the performance for multiple methods.
#'
#' @details
#' This function parses the returned list object from function \code{\link{doCV}} and extracts the performance data for each classification method. It assembles the data and generates either a ROC curve for each method (binary classification) or a boxplot representation of the multiclass AUC values gathered through the CV. 
#' @param retCV Return list from function \code{\link{doCV}}
#' @return void
#' @seealso \code{\link{doCV}}
resultCV <- function(retCV) {
	## for each element in retCV, extract the performance object and plot a roc curve or auc boxplot

	allgroups <- unique(sapply(retCV, function(x) names(x$performance)))
	if(length(allgroups)!=1) {
		stop("Error: different grouping factors were analysed.")
	}

	for(j in 1:length(allgroups)) {
		grp <- allgroups[j]
		print(paste("Generating CV result for group", grp))
		allaucs <- NULL
		## select the method
		for(i in 1:length(retCV)) {
			ret <- retCV[[i]]
			rn <- names(retCV)[i]
			pr <- ret$performance[[grp]]
			fitted <- pr$fitted
			labels <- pr$labels
			Ncl <- length(unique(as.character(unlist(labels))))
			aucs <- pr$aucs
			
			#if(is.null(roc.curve)) {
			if(Ncl>2) {
				## attach AUCs to a matrix and plot later
				allaucs <- cbind(allaucs, aucs)
			} else {
				## generate the roc curve for one method
				pred <- prediction(as.vector(unlist(fitted)), as.vector(unlist(labels)))
				roc.curve <- performance(pred, "tpr", "fpr")
				aucs <- unlist(performance(pred, "auc")@y.values)
				auc <- signif(median(aucs), digits=3)
				plot(roc.curve, avg="none", spread.estimate="none", sub=paste("AUC:", auc), main=rn, lwd=2.5)
				roc_binterval(fitted, labels)
			}
		}

		## make the AUC - Boxplot for multiclass predictions
		if(!is.null(allaucs)) {
			colnames(allaucs) <- names(retCV)
			boxplot(allaucs, ylim=c(0,1), xlim=c(0.5,(ncol(allaucs)+1)), main=c(paste(Ncl, "- class classification"), "multinomial model"))
			#axis(1, at=1, labels="multiclass AUC")
			#allcl <- sort(unique(as.character(unlist(labels))))
			allcl <- pr$classes
			legend("bottomright", border="white", fill="white", legend=c("classes:",allcl), bty="n")
			aucmed <- median(allaucs, na.rm=TRUE)
			aucmad <- mad(allaucs, na.rm=TRUE)
			abline(h=aucmed, col="darkred", lty=2)
			abline(h=aucmed+aucmad, col="darkred", lty=3)
			abline(h=aucmed-aucmad, col="darkred", lty=3)
			text(ncol(allaucs)+0.7, aucmed+0.01, labels="median auc", col="darkred", cex=0.7)
		}
	}
	
}
