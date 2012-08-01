	cvGBM <- function(logX, groupings, DIR, 
				params=list(seed=123, ncv=5, repeats=10, 
							ntree=1000, shrinkage=0.01, interaction.depth=3,
							bag.fraction=0.75, train.fraction=0.75,
							n.minobsinnode=3, n.cores=1, verbose=TRUE,
							jitter=FALSE))
	{
		fs.method <- "gbm"
		seed <- params$seed
		ncv <- params$ncv
		repeats <- params$repeats
		#maxRuns <- params$maxRuns
		jitter <- params$jitter
	
#~ 		ntree <- params$ntree
#~ 		shrinkage <- params$shrinkage
#~ 		interaction.depth <- params$interaction.depth
#~ 		bag.fraction <- params$bag.fraction
#~ 		train.fraction <- params$train.fraction
#~ 		n.minobsnode <- params$n.minobsnode
#~ 		#cv.folds <- params$cv.folds
#~ 		n.cores <- params$n.cores
#~ 		verbose <- params$verbose
#~ 		
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
        ## and package parallel is there
        useparallel <- length(grep("package:(parallel|multicore)", search())>0)
        if(length(X)>1 & useparallel) {
			resGBM <- mclapply(X, cv_gbmclass, logX=logX, ncv=ncv, repeats=repeats, seed=seed, params=params, mc.preschedule=TRUE, mc.cores=length(X))
		} else {
			resGBM <- lapply(X, cv_gbmclass, logX=logX, ncv=ncv, repeats=repeats, seed=seed, params=params)
		}

		#rrr <- rfclass_cv(X[["groupings"]], logX=logX, ncv=ncv, repeats=repeats, seed=seed, maxRuns=maxRuns)
		#resRF <- list(ttype=rrr)
	#############
	## TODO
		featlist <- sapply(resGBM, function(x) x$selprobes) #extract_features_gbm(resGBM, SUBDIR)
		save(resGBM, X, logX, fs.method, SUBDIR, featlist, file=paste(SUBDIR, "env.RData", sep="/"))

		list(res=resGBM, featlist=featlist)
	}
