run_pam <- function(pamdat, nfold=5, n.threshold=30, seed=NULL) {
		#stopifnot(require(pamr))
		if(is.null(seed))
			seed <- as.numeric(strsplit(as.character(Sys.time()), ":")[[1]][3])
		set.seed(seed)
		max_allowed_feat <- pamdat$max_allowed_feat
		#pamdat <- pamdat[-3] ## remove the max_allowed_feat stuff
		histtr <- pamr.train(pamdat, n.threshold=n.threshold)
		histcv <- pamr.cv(histtr, pamdat, nfold=nfold)
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
