	cvRFBORUTA <- function(logX, groupings, DIR, params=list(seed=123, ncv=5, repeats=10,maxRuns=300)) {

		fs.method <- "rf_boruta"
		seed <- params$seed
		ncv <- params$ncv
		repeats <- params$repeats
		maxRuns <- params$maxRuns
		jitter <- params$jitter
		
		## introduce some minimal noise to make scaling etc. possible
		if(jitter) {
			logX <- jitter(logX)
		}
		
		SUBDIR <- paste(DIR,fs.method,sep="/")
		if(!file.exists(SUBDIR))
			dir.create(SUBDIR)

	  	fnames <- paste(SUBDIR, "/", names(groupings), ".pdf", sep="")
		X <- lapply(1:length(groupings), function(i,groupings,fnames) list(groupings[[i]], fnames[i]), groupings=groupings, fnames=fnames)
		names(X) <- names(groupings)

        ## use multicores if more than one group is to be classified
        useparallel <- length(grep("package:(parallel|multicore)", search())>0)
        if(length(X)>1 & useparallel) {
			resRF <- mclapply(X, rfclass_cv, logX=logX, ncv=ncv, repeats=repeats, seed=seed, maxRuns=maxRuns, mc.preschedule=TRUE, mc.cores=length(X))
		} else {
			resRF <- lapply(X, rfclass_cv, logX=logX, ncv=ncv, repeats=repeats, seed=seed, maxRuns=maxRuns)
		}

		#rrr <- rfclass_cv(X[["groupings"]], logX=logX, ncv=ncv, repeats=repeats, seed=seed, maxRuns=maxRuns)
		#resRF <- list(ttype=rrr)
		
		featlist <- extract_features_rf_boruta(resRF, SUBDIR)
		save(resRF, X, logX, fs.method, SUBDIR, featlist, file=paste(SUBDIR, "env.RData", sep="/"))

		list(res=resRF, featlist=featlist)
	}
