cvRFBORUTA <- function(logX, groupings, DIR, params=NULL) {
	#list(seed=123, ncv=5, repeats=10,maxRuns=300, rfimportance="MeanDecreaseGini", ntree=1000, localImp=TRUE, fs.method="rf_boruta", savres=FALSE)) {
	if(is.null(params)) {
		params <- control_params()
	}
	fs.method <- params$fs.method #"rf_boruta"
	seed <- params$seed
	ncv <- params$ncv
	repeats <- params$repeats
	maxRuns <- params$maxRuns
	jitter <- params$jitter
	rfimportance <- params$rfimportance
	ntree <- params$ntree
	fs.method <- params$fs.method
	localImp <- params$localImp
	saveres <- params$saveres
	
	## introduce some minimal noise to make scaling etc. possible
	if(jitter) {
		logX <- jitter(logX)
	}
	
	## create an output folder if results should be saved
	if(saveres & !is.null(DIR)) {
		SUBDIR <- paste(DIR,fs.method,sep="/")
		if(!file.exists(SUBDIR))
			dir.create(SUBDIR)
		fnames <- paste(SUBDIR, "/", names(groupings), ".pdf", sep="")
	} else {
		SUBDIR <- NULL
		fnames <- rep("-", length(groupings)) #paste(names(groupings), ".pdf", sep="")
	}

	X <- lapply(1:length(groupings), function(i,groupings,fnames) list(groupings[[i]], fnames[i]), groupings=groupings, fnames=fnames)
	names(X) <- names(groupings)

	## use multicores if more than one group is to be classified
	useparallel <- length(grep("package:(parallel|multicore)", search())>0)
	if(length(X)>1 & useparallel) {
		resRF <- mclapply(X, cv_rfclass, logX=logX, ncv=ncv, repeats=repeats, seed=seed, maxRuns=maxRuns, rfimportance=rfimportance, ntree=ntree, fs.method=fs.method, localImp=localImp, mc.preschedule=TRUE, mc.cores=length(X))
	} else {
		resRF <- lapply(X, cv_rfclass, logX=logX, ncv=ncv, repeats=repeats, seed=seed, maxRuns=maxRuns, rfimportance=rfimportance, ntree=ntree, fs.method=fs.method, localImp=localImp)
	}

	#rrr <- rfclass_cv(X[["groupings"]], logX=logX, ncv=ncv, repeats=repeats, seed=seed, maxRuns=maxRuns)
	#resRF <- list(ttype=rrr)

	## extract the performance objects
	performance <- lapply(resRF, function(x) x$performance)
	names(performance) <- names(X)

	## is this really needed? should probably not be written without asking...
	featlist <- extract_features_rf_boruta(resRF, SUBDIR)
	if(saveres & !is.null(SUBDIR)) {
		save(resRF, X, logX, fs.method, rfimportance, ntree, localImp, SUBDIR, featlist, file=paste(SUBDIR, "env.RData", sep="/"))
	}

	list(res=resRF, featlist=featlist, performance=performance)
}
