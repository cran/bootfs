roc <- function(pr,labels,measure="tpr",x.measure="fpr",colorize=TRUE, avg="none", spread.estimate="stddev", filter=NULL) {
	#stopifnot(require(ROCR))
	pred <- prediction(pr, labels)
	perf <- performance(pred, measure = measure, x.measure = x.measure)
	auc <- signif(mean(unlist(performance(pred, measure = "auc")@y.values)),digits=3)
	if(!is.null(filter)) {
		if(auc>=filter) {
			plot(perf, colorize=colorize, avg=avg, spread.estimate=spread.estimate, lwd=2.5)
			text(0.6,0,labels=paste("mean AUC: ", auc))
		}
	}
	invisible(auc)
}

roc_binterval <- function(fitted, labels) {
	fvec <- as.vector(unlist(fitted))
	lvec <- as.vector(unlist(labels))
	for(i in 1:50) {
		sx <- sample(1:length(fvec), length(fvec), replace=TRUE)
		fvecx <- fvec[sx]
		lvecx <- lvec[sx]
		pred <- prediction(fvecx, lvecx)
		roc.curve <- performance(pred, "tpr", "fpr")
		## since we concatenate each yhat/yreal pair of values,
		## peformance(pred,"auc")@y.values always has exactly one
		## element, so we use this for auc calculation
		#auc <- signif(performance(pred, "auc")@y.values[[1]], digits=3)
		#aucs <- unlist(performance(pred, "auc")@y.values)
		#auc <- signif(median(aucs), digits=3)
		plot(roc.curve, avg="none", spread.estimate="none", add=TRUE, col="#31313122")
	}
	
}
