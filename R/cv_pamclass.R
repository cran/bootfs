cv_pamclass <-
function(X, logX, nfold=5, n.threshold=30, seed=NULL, max_allowed_feat=NULL, repeats=1) {
		Y <- X[[1]]
		filename <- X[[2]]
		nas <- which(is.na(Y))
		if(length(nas) > 0) {
			ypam <- Y[-nas]
			xpam <- t(logX[-nas,])
		} else {
			ypam <- Y
			xpam <- t(logX)
		}
		pamdat <- list(x=xpam, y=ypam, max_allowed_feat=max_allowed_feat)
		pamret <- list()
		set.seed(seed)
		for(ri in 1:repeats) {
			pamret[[ri]] <- run_pam(pamdat, nfold=nfold, n.threshold=n.threshold, seed=seed)
		}
		pamret
	}


run_pam <- function(pamdat, nfold=5, n.threshold=30, seed=NULL) {
		#stopifnot(require(pamr))
		#if(is.null(seed))
		#	seed <- as.numeric(strsplit(as.character(Sys.time()), ":")[[1]][3])
		#set.seed(seed)
		max_allowed_feat <- pamdat$max_allowed_feat
		
		#pamdat <- pamdat[-3] ## remove the max_allowed_feat stuff
		histtr <- pamr.train(pamdat, n.threshold=n.threshold)
		## define the folds variable
		#folds <- select_cv_balanced(pamdat$y, nfold)
		nottwoclasses <- TRUE
		while(nottwoclasses) {
			folds <- createFolds(pamdat$y, k=nfold, returnTrain=FALSE) ## from caret package
			nottwoclasses <- any(sapply(folds, function(x, yp) length(unique(yp[x]))<2, yp=pamdat$y))
		}
		#folds <- createFolds(pamdat$y, k=nfold, returnTrain=FALSE) ## from caret package
		histcv <- pamr.cv(histtr, pamdat, folds=folds)
		#histcv <- pamr.cv(histtr, pamdat, nfold=nfold)
		ts <- histcv$threshold
		te <- histcv$error

		if(!is.null(max_allowed_feat)) {
			raus <- which(histcv$size>max_allowed_feat)
			if(length(raus>0)) {
				ts <- ts[-raus]
				te <- te[-raus]
			}
		}
		## select threshold
		tmin <- ts[min(which(te==min(te)))] ## prefer smallest feature number in case of equal errors
		#tmin <- ts[min(which(histcv$error==min(histcv$error)))]
		minerr <- histtr$errors[which(histtr$threshold==tmin)] / nrow(histtr$yhat) * 100
		selected <- pamr.predict(histtr, pamdat$x, tmin, type="nonzero")
		selected_names <- rownames(pamdat$x)[selected]
		list(trained=histtr, cv=histcv, tmin=tmin, minerr=minerr, selected=selected, selected_names=selected_names)
	}



