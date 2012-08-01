	cvPAMR <- function(logX, groupings, DIR, params=list(seed=123, ncv=5, repeats=10,max_allowed_feat=500, n.threshold=50)) {
		fs.method <- "pamr"
		seed <- params$seed
		ncv <- params$ncv
		repeats <- params$repeats
		max_allowed_feat <- params$max_allowed_feat
		n.threshold <- params$n.threshold
		jitter <- params$jitter
		
		## introduce some minimal noise to make scaling etc. possible
		if(jitter) {
			logX <- jitter(logX)
		}
		
		SUBDIR <- paste(DIR,fs.method,sep="/")
		if(!file.exists(SUBDIR))
			dir.create(SUBDIR)

		#X <- list(groupings=list(groupings, paste(SUBDIR, "groupings.pdf", sep="/")))
		fnames <- paste(SUBDIR, "/", names(groupings), ".pdf", sep="")
		X <- lapply(1:length(groupings), function(i,groupings,fnames) list(groupings[[i]], fnames[i]), groupings=groupings, fnames=fnames)
		names(X) <- names(groupings)

        ## use multicores if more than one group is to be classified
        useparallel <- length(grep("package:(parallel|multicore)", search())>0)
        if(length(X)>1 & useparallel) {
			resPAM <- mclapply(X, pamclass, logX=logX, nfold=ncv, n.threshold=n.threshold, seed=seed, max_allowed_feat=max_allowed_feat, mc.preschedule=TRUE, mc.cores=length(X))
		} else {
			resPAM <- lapply(X, pamclass, logX=logX, nfold=ncv, n.threshold=n.threshold, seed=seed, max_allowed_feat=max_allowed_feat)
		}
		## make the feature table
		featlist <- extract_feat_pam(resPAM, SUBDIR)

		save(resPAM, X, logX, ncv, n.threshold, fs.method, SUBDIR, featlist, file=paste(SUBDIR, "env.RData", sep="/"))

		################################
		## ROC Curves for PAM
		pdf(paste(SUBDIR, "PAMR_ROC_Curves.pdf", sep="/")) #, width=10, height=10)
		#par(mfrow=c(3,3))
		for(i in 1:length(X)) {
			obj <- resPAM[[i]]$cv
				
			yhat <- yreal <- NULL
			for(j in 1:length(obj$folds)) {
				fold <- obj$folds[[1]]
				th <- resPAM[[i]]$tmin
				thind <- which(obj$threshold==th)
				## get the curves for each fold, for all classes and the best threshold
				#probabilities of classifying the sample in the positive class
				yhat <- cbind(yhat, as.vector(obj$prob[fold, 2, thind]))
				yreal <- cbind(yreal,as.numeric(as.character(obj$y))[fold])
			}
			pred <- prediction(yhat, yreal)
			roc.curve <- performance(pred, "tpr", "fpr")
			auc <- signif(performance(pred, "auc")@y.values[[1]], digits=3)
			plot(roc.curve, avg="threshold", spread.estimate="none", sub=paste("AUC:", auc), main=names(X)[i])
		}
		dev.off()

		list(res=resPAM, featlist=featlist)
	}
