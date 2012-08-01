## In this file a set of functions for the bootstrapping procedure are collected.
## These functions are the elemental calls which can be parallelized


## for SVM classification bootstrapping
bstr_multi <- function(X, datX, bstr, seed=123, fs.method="scad",
				bounds=NULL, maxiter=1000, maxevals=500) {
	datY <- X[[1]] # select the strat vector
	nasY <- which(is.na(datY))
	nasX <- which(apply(datX, 1, function(x) all(is.na(x))))
	nas <- unique(c(nasY, nasX))
	if(length(nas)>0) {
		datX <- datX[-nas,]
		datY <- datY[-nas]
	}
	
	## format such that binary vector is -1/1
	if(!all(as.character(sort(levels(factor(datY))))==c("-1","1"))) {
		datY <- ifelse(as.numeric(factor(datY))==1, -1, 1)
	}
	
	## datY must be numeric
	datY <- as.numeric(as.character(datY))
	
	## somehow this is the only way how these methods are going to work
	grid.search <- "interval"
	lambda1.scad <- lambda2.scad <- NULL
	bounds <- NULL

#~         if(fs.method %in% c("scad+L2", "DrHSVM")) {
#~ 			grid.search <- "discrete"
#~ 			#lambda1.scad <- c(seq(0.01 ,0.05, .01),  seq(0.1,0.5, 0.2), 1 ) 	
#~ 			#lambda1.scad <- lambda1.scad[2:3]
#~ 			lambda1.scad <- c(0.01, 0.05, 0.1, 0.25, 0.5, 1)
#~ 			bounds <- NULL
#~ 		} else {
#~ 			grid.search <- "interval"
#~ 			bounds <- t(data.frame(log2lambda1=c(-10, 10)))
#~ 			colnames(bounds) <- c("lower", "upper")
#~ 			lambda1.scad <- NULL
#~ 		}

	bind <- createResample(datY, bstr)

	##dat_bstr <- createResample(datY, times=bstr)
	scads <- list()
	for(rp in 1:bstr) {
		print(rp)
		#if(!is.null(seed)) { ## increase and set seed for each iteration
			seed <- seed + 1
		#	set.seed(seed)
		#}
		proceed <- TRUE
		while(proceed) {
			## bootstrap data: keep proportions of 1 and -1 in all sets
			#dat_bstr <- select_bootstrap_data(datX, datY)
			dat_bstr <- list(datX=datX[bind[[rp]],], datY=datY[bind[[rp]]])
			rownames(dat_bstr$datX) <- paste(rownames(dat_bstr$datX), 1:nrow(dat_bstr$datX), sep=".")
			ttt <- try(scad <- svm.fs(dat_bstr[[1]], y=dat_bstr[[2]], 
							fs.method=fs.method, bounds=bounds,
							lambda1.set=lambda1.scad, lambda2.set=lambda2.scad,
							cross.outer= 0, grid.search = grid.search,  maxIter = maxiter, 
							inner.val.method = "cv", cross.inner= 5, maxevals=maxevals,
							seed=seed, parms.coding = "log2", show="none", verbose=FALSE )
						)
			if(class(ttt)!="try-error")
				proceed <- FALSE
		}
		scads[[rp]] <- scad
	}
	scads
}


## for random forest (Boruta) bootstrapping
rf_multi <- function(X, datX, maxRuns=500, seed=123, bstr=100, ntree=500, localImp=TRUE, rfimportance="MeanDecreaseGini", fs.method="rf_boruta") {
	datY <- X[[1]]
	nasY <- which(is.na(datY))
	nasX <- which(apply(datX, 1, function(x) all(is.na(x))))
	nas <- unique(c(nasY, nasX))
	if(length(nas)>0) {
		datX <- datX[-nas,]
		datY <- datY[-nas]
	}
	bind <- createResample(datY, bstr)
	#sapply(bind, function(ii, datY) table(datY[ii]), datY=datY)
	#bind <- createResample(Y, bstr)
	#sapply(bind, function(ii, Y) table(Y[ii])/length(Y)*100, Y=Y)

	rfs <- list()
	for(rp in 1:bstr) {
		print(rp)
		#if(!is.null(seed)) { ## increase and set seed for each iteration
			seed <- seed + 1
		#	set.seed(seed)
		#}
		## bootstrap data: keep proportions of 1 and -1 in all sets
		#dat_bstr2 <- select_bootstrap_data(datX, datY)
		dat_bstr <- list(datX=datX[bind[[rp]],], datY=datY[bind[[rp]]])
		rownames(dat_bstr$datX) <- paste(rownames(dat_bstr$datX), 1:nrow(dat_bstr$datX), sep=".")

		if(fs.method=="rf_boruta") {
			bres <- Boruta(x=as.data.frame(dat_bstr$datX), y=factor(dat_bstr$datY), doTrace=2, maxRuns=maxRuns)
			selprobes <- gsub("`","",names(bres$finalDecision[which(bres$finalDecision!="Rejected")]))
		} else {
			#browser()
			## use a default random forest: minimum necessary selection
			bres <- randomForest(x=as.data.frame(dat_bstr$datX), y=factor(dat_bstr$datY),  xtest=NULL, ytest=NULL, ntree=ntree, keep.forest=TRUE, proximity=TRUE, localImp=localImp)
			imp <- importance(bres)
			ord <- order(imp[,rfimportance], decreasing=TRUE)
			imp <- imp[ord,]
			selprobes <- rownames(imp)[which(imp[,rfimportance]>=1)] ## ranked 
		}
		rfs[[rp]] <- list(bres=bres, selprobes=selprobes)
	}
	rfs
}


## for GBM bootstrapping
gbm_multi <- function(X, datX, seed=123, bstr=100, params=NULL) {
	datY <- X[[1]]
	nasY <- which(is.na(datY))
	nasX <- which(apply(datX, 1, function(x) all(is.na(x))))

	ntree <- params$ntree
	shrinkage <- params$shrinkage
	interaction.depth <- params$interaction.depth
	bag.fraction <- params$bag.fraction
	train.fraction <- params$train.fraction
	n.minobsinnode <- params$n.minobsinnode
	verbose <- params$verbose

	## necessary to remove NAs?
	nas <- unique(c(nasY, nasX))
	if(length(nas)>0) {
		datX <- datX[-nas,]
		datY <- datY[-nas]
	}

	#	browser()

	bind <- createResample(datY, bstr)

	ret <- list()
	for(rp in 1:bstr) {
		print(rp)
		#if(!is.null(seed)) { ## increase and set seed for each iteration
			seed <- seed + 1
		#	set.seed(seed)
		#}
		## bootstrap data: keep proportions of 1 and -1 in all sets
		#dat_bstr <- select_bootstrap_data(datX, datY)
		dat_bstr <- list(datX=datX[bind[[rp]],], datY=datY[bind[[rp]]])
		rownames(dat_bstr$datX) <- paste(rownames(dat_bstr$datX), 1:nrow(dat_bstr$datX), sep=".")

		## fit the model on the bootstrap set
		gbmres <- fitGBM(as.data.frame(dat_bstr$datX), dat_bstr$datY, ntree, shrinkage, interaction.depth, bag.fraction, train.fraction, n.minobsinnode, verbose)
		## extract the model
		model <- gbmres$model
		imp <- summary(model, plotit=FALSE)
		selprobes <- rownames(imp)[which(imp[,"rel.inf"]>0)]
		
		ret[[rp]] <- list(fit=gbmres, selprobes=selprobes)
	}
	ret
}
