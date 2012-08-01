	cvPAMR <- function(logX, groupings, DIR, params=list(seed=123, ncv=5, repeats=10,max_allowed_feat=500, n.threshold=50)) {
		fs.method <- "pamr"
		seed <- params$seed
		ncv <- params$ncv
		repeats <- params$repeats
		max_allowed_feat <- params$max_allowed_feat
		n.threshold <- params$n.threshold
		jitter <- params$jitter
		
		## introduce some minimal noise to make scaling etc. possible
		## might be an issue if lots of zero values, e.g. in NGS experiments
		## occur.
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
			resPAM <- mclapply(X, cv_pamclass, logX=logX, nfold=ncv, n.threshold=n.threshold, seed=seed, max_allowed_feat=max_allowed_feat, repeats=repeats, mc.preschedule=TRUE, mc.cores=length(X))
		} else {
			resPAM <- lapply(X, cv_pamclass, logX=logX, nfold=ncv, n.threshold=n.threshold, seed=seed, max_allowed_feat=max_allowed_feat, repeats=repeats)
		}
		## make the feature table
		featlist <- sapply(resPAM, extract_feat_pam, SUBDIR=SUBDIR)

		save(resPAM, X, logX, ncv, n.threshold, fs.method, SUBDIR, featlist, file=paste(SUBDIR, "env.RData", sep="/"))

		################################
		## ROC Curves for PAM
		pdf(paste(SUBDIR, "PAMR_ROC_Curves.pdf", sep="/")) #, width=10, height=10)
		#par(mfrow=c(3,3))
		for(i in 1:length(X)) {
			## find the distribution type
			clvec <- X[[i]][[1]]
			raus <- which(is.na(clvec))
			if(length(raus)>0) {
				clvec <- clvec[-raus]
			}
			if(length(unique(clvec))==2) {
				distribution <- "bernoulli"
			} else {
				distribution <- "multinomial"
			}
			#obj <- resPAM[[i]]$cv
			rpam <- resPAM[[i]]
			yhat <- yreal <- list()# NULL
			it <- 0
			for(ri in 1:repeats) {
				obj <- rpam[[ri]]$cv
				yhat.rep <- yreal.rep <- NULL
				for(j in 1:length(obj$folds)) {
					it <- it+1
					fold <- obj$folds[[j]]
					th <- rpam[[ri]]$tmin
					#th <- resPAM[[i]]$tmin
					thind <- which(obj$threshold==th)
					if(distribution=="multinomial") {
						xpred <- obj$prob[fold,,thind]
						Ncl <- ncol(xpred)
						kx <- 0:Ncl * 2
						argmax <- apply(xpred, 1, function(x) which(x==max(x)))
						allmax <- apply(xpred, 1, max)
						pred <- kx[argmax] + allmax
						# attach the prediction probability for the classes. Note that these are one probability for each class, i.e. a matrix with n.class columns for each fold/repeat
						#fitted <- cbind(fitted, pred)
						yhat.rep <- c(yhat.rep, pred)
						yreal.rep <- c(yreal.rep,as.numeric(obj$y)[fold])
					} else {
						## get the curves for each fold, for all classes and the best threshold
						#probabilities of classifying the sample in the positive class
						yhat.rep <- c(yhat.rep, as.vector(obj$prob[fold, 2, thind]))
						yreal.rep <- c(yreal.rep,as.numeric(as.character(obj$y))[fold])
					}
					
				}
				## combine the repeats in one single table
				#yhat <- cbind(yhat, yhat.rep)
				#yreal <- cbind(yreal, yreal.rep)
				yhat[[it]] <- yhat.rep
				yreal[[it]] <- yreal.rep
			}

			if(distribution=="multinomial") {
				aucs <- vector("numeric", length(yhat))	
				for(ki in 1:length(yreal)) {
					grpx <- yreal[[ki]]
					predx <- yhat[[ki]]
					aucs[ki] <- multiclass.roc(grpx~predx)$auc
				}
				Y <- X[[i]][[1]]
				## plot auc distribution
				boxplot(aucs, ylim=c(0,1), main=c(paste(Ncl, "- class classification"), "multinomial model"))
				axis(1, at=1, labels="multiclass AUC")
				legend("bottomright", border="white", fill="white", legend=c("classes:",levels(Y)))
			} else {
				## make the roc curve
				pred <- prediction(as.vector(unlist(yhat)), as.vector(unlist(yreal)))
				roc.curve <- performance(pred, "tpr", "fpr")
				## since we concatenate each yhat/yreal pair of values,
				## peformance(pred,"auc")@y.values always has exactly one
				## element, so we use this for auc calculation
				#auc <- signif(performance(pred, "auc")@y.values[[1]], digits=3)
				aucs <- unlist(performance(pred, "auc")@y.values)
				auc <- signif(median(aucs), digits=3)
				plot(roc.curve, avg="none", spread.estimate="none", sub=paste("AUC:", auc), main=names(X)[i], lwd=2.5)
				roc_binterval(yhat, yreal)
			}

		}
		dev.off()

		list(res=resPAM, featlist=featlist)
	}
