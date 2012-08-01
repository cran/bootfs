bootstrap_roc <-
function(yhat, yreal, N=500) {
		aucs <- vector("numeric", N)
		preds <- list()
		for(i in 1:N) {
			bsel <- sample(1:length(yhat), length(yhat), replace=TRUE)
			yhatX <- yhat[bsel]
			yrealX <- yreal[bsel]
			pred <- prediction(yhatX, yrealX)
			preds[[i]] <- pred
			roc.curve <- performance(pred, "tpr", "fpr")
			auc <- signif(performance(pred, "auc")@y.values[[1]], digits=3)
			if(i==1) {
			   plot(roc.curve, avg="none", spread.estimate="none", col="#B0B0B055") #, main=names(X)[i])
			} else {
			   plot(roc.curve, avg="none", spread.estimate="none", add=TRUE, col="#B0B0B055") # main=names(X)[i], add=TRUE)
			}
			aucs[i] <- auc
			preds[[i]] <- pred
		}
		list(aucs=aucs, preds=preds)
	}
