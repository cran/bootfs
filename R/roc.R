roc <- function(pr,labels,measure="tpr",x.measure="fpr",colorize=TRUE, avg="none", spread.estimate="stddev", filter=NULL) {
	#stopifnot(require(ROCR))
	pred <- prediction(pr, labels)
	perf <- performance(pred, measure = measure, x.measure = x.measure)
	auc <- signif(mean(unlist(performance(pred, measure = "auc")@y.values)),digits=3)
	if(!is.null(filter)) {
		if(auc>=filter) {
			plot(perf, colorize=colorize, avg=avg, spread.estimate=spread.estimate)
			text(0.6,0,labels=paste("mean AUC: ", auc))
		}
	}
	invisible(auc)
}
