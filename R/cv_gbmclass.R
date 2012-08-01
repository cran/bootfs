cv_gbmclass <-
function(X, logX, ncv=5, repeats=10, seed=123, params,
			avg="none", spread.estimate="none",
			colorize=FALSE, minclassN=NULL, ...)
{
	ntree <- params$ntree
	shrinkage <- params$shrinkage
	interaction.depth <- params$interaction.depth
	bag.fraction <- params$bag.fraction
	train.fraction <- params$train.fraction
	n.minobsinnode <- params$n.minobsinnode
	#cv.folds <- params$cv.folds
	n.cores <- params$n.cores
	verbose <- params$verbose
	
	Y <- X[[1]]
	filename <- X[[2]]
	nas <- which(is.na(Y))
	if(length(nas)>0) {
		x <- as.data.frame(logX[-nas,])
		yp <- Y[-nas]
	} else {
		x <- as.data.frame(logX)
		yp <- Y
	}

	## CV
	#library(ROCR)
	#ncv <- 5
	cvby <- ceiling(nrow(x)/ncv) # round up
	## initialize the result objects
	sn <- sp <- testdim <- NULL
	fitted <- labels <- list()
	predlist <- fitlist <- testlist <- features <- gbmlist <- list()
	it <- 0
	set.seed(seed)
	for(ri in 1:repeats) {
		## find a permutation leaving stratified test/training sets
		## with regard to the class label distributions
		nottwoclasses <- TRUE
		while(nottwoclasses) {
			folds <- createFolds(yp, ncv, returnTrain=FALSE) ## from caret package
			nottwoclasses <- any(sapply(folds, function(x, yp) length(unique(yp[x]))<2, yp=yp))
		}

		for(i in 1:ncv) {

			it <- it + 1
			sel <- folds[[i]]
			seltrain <- setdiff(1:nrow(x), sel)
			#############
			## Workaround for very small sample sets, only
			## effective if minclassN is set explicitely
			## if the number of class members is very small,
			## reuse samples it in the training
			## however: test and training sets will not be confused
			## Note: one should make sure that this is not happening,
			##       this workaround is only for ensuring that the
			##       algorithm runs even for small samples
			#############
			if(!is.null(minclassN)) {
				if(min(table(yp[-sel]))<minclassN) {            
					ypsmall <- table(yp[-sel])
					cltoadd <- names(ypsmall)[which(ypsmall<minclassN)]
					for(clta in cltoadd) {
						tosmallcl <- which(yp[seltrain]==clta)
						repl <- sample(seltrain[tosmallcl],minclassN, replace=TRUE)
						seltrain <- seltrain[-tosmallcl]
						seltrain <- c(seltrain, repl)
					}
					maxRuns <- 50
					warning("Some class only has very few members. Extending the class for training and reducing maxRuns to 50")
				}
			}
			## define the test and training sets
			test <- x[sel,]
			testg <- yp[sel]
			train <- x[seltrain,]
			traing <- yp[seltrain]
		
			# create a try error object to initialise the loop
			# below -> can this be done more elegantly?
			btr <- try(silent=TRUE)
			tecnt <- 0 # try-error count
			while(inherits(btr,"try-error")) {
				print(tecnt)
				tecnt <- tecnt+1
				## try 10 times, if no success, stop it
				if(tecnt>10)
					stop("GBMRF Error: no data in one group")
				# fit initial model
				btr <- try(fit <- fitGBM(train, traing, ntree, shrinkage, interaction.depth, bag.fraction, train.fraction, n.minobsinnode, verbose))
			}
			distribution <- fit$distribution
			form <- fit$form
			gbm1 <- fit$model
			# check performance using a 50% heldout test set
			best.iter <- gbm.perf(gbm1,method="test", plot.it=FALSE)
			print(best.iter)
			importance <- summary(gbm1,n.trees=best.iter, plotit=FALSE)
			selprobes <- as.character(importance[which(importance[,"rel.inf"]>0),"var"])
			tdata <- data.frame(test)
			###########################
			if(distribution=="bernoulli") {
				gbmpred <- predict(gbm1, newdata=tdata, ntrees=best.iter, type="link")
				#fitted <- cbind(fitted, gbmpred) # attach the prediction probability for the positive class
				fitted[[it]] <- gbmpred # attach the prediction probability for the positive class
				pred <- gbmpred
			###########################
			} else {
			###########################
				## multinomial case
				gbmpred <- predict(gbm1, newdata=tdata, ntrees=best.iter, type="response")
				Ncl <- ncol(gbmpred)
				kx <- 0:Ncl * 2
				argmax <- apply(gbmpred, 1, function(x) which(x==max(x)))
				allmax <- apply(gbmpred, 1, max)
				pred <- kx[argmax] + allmax
				 # attach the prediction probability for the classes. Note that these are one probability for each class, i.e. a matrix with n.class columns for each fold/repeat
				#fitted <- cbind(fitted, pred)
				fitted[[it]] <- pred
			}
			###########################
			#labels <- cbind(labels, testg)
			labels[[it]] <- testg
			predlist[[it]] <- pred ## class prediciton vector, can be used in multiclass.roc
			#fitlist[[it]] <- gbm1 ## model fitted on subset of variables???
			testlist[[it]] <- gbmpred ## prediction objects
			features[[it]] <- selprobes
			gbmlist[[it]] <- gbm1 ## gbm model fits
		} # cv loop
	} # repeat loop

#browser()
	## create ROC curves for performance evaluation -> how to do for 3 or more classes? use 				multiclass.roc(testg~pred)
	if(!is.null(filename)) {
		pdf(filename)
	}
	if(distribution=="bernoulli") {
		#auc <- roc(fitted,labels,measure="tpr",x.measure="fpr",colorize=colorize, avg=avg, spread.estimate=spread.estimate, filter=1)
		#title(main="ROC curves for each CV run")
		auc <- roc(as.vector(unlist(fitted)),as.vector(unlist(labels)),measure="tpr",x.measure="fpr",colorize=colorize, avg="none", spread.estimate="none", filter=0)
		title(main="ROC curves averaged over all CV runs")
		roc_binterval(fitted, labels)
	} else {
		aucs <- vector("numeric", ncol(fitted))	
		for(ki in 1:ncol(labels)) {
			grpx <- labels[,ki]
			predx <- fitted[,ki]
			aucs[ki] <- multiclass.roc(grpx~predx)$auc
		}
		## plot auc distribution
		boxplot(aucs, ylim=c(0,1), main=c(paste(Ncl, "- class classification"), "multinomial model"))
		axis(1, at=1, labels="multiclass AUC")
		legend("bottomright", border="white", fill="white", legend=c("classes:",levels(Y)))
	}
	if(!is.null(filename)) {
		dev.off()
	}

	cvobj <- list(fitted=fitted, labels=labels, fitlist=fitlist, testlist=testlist, features=features, gbmlist=gbmlist)
##TODO!!!	## train on all data
	fit <- fitGBM(x, yp, ntree, shrinkage, interaction.depth, bag.fraction, train.fraction, n.minobsinnode, verbose) 
	distribution <- fit$distribution
	form <- fit$form
	gbm1 <- fit$model
	# check performance using a 50% heldout test set
	best.iter <- gbm.perf(gbm1,method="test", plot.it=FALSE)
	print(best.iter)
	importance <- summary(gbm1,n.trees=best.iter, plotit=FALSE)
	selprobes <- as.character(importance[which(importance[,"rel.inf"]>0),"var"])
	
	## ERROR ESTIMATE?? CONFUSIONMATRIX??
	#err <- round(randf$err.rate[randf$ntree, "OOB"] * 100, digits = 2)
	#err <- sum(rowSums(cbind(yp, as.numeric(as.character(predict(randf, x)))))==0)*100
	list(model=gbm1, selprobes=selprobes, cvobj=cvobj) #err=err,
}


fitGBM <- function(train, traing, ntree, shrinkage, interaction.depth, bag.fraction, train.fraction, n.minobsinnode, verbose) {
	form <- as.formula(paste("traing", paste(colnames(train),collapse="+"), sep="~"))
	data <- data.frame(train)
	data[["traing"]] <- factor(traing)
	clvec <- traing
	raus <- which(is.na(clvec))
	if(length(raus)>0) {
		clvec <- clvec[-raus]
	}

	if(length(unique(clvec))==2) {
		distribution <- "bernoulli"
		#data$traing <- factor(ifelse(traing==1, 1, 0))
		data$traing <- ifelse(traing==1, 1, 0)
	} else {
		distribution <- "multinomial"
	}
	if(verbose)
		print(paste("Using", distribution, "distribution as response"))
	btr <- try(gbm1 <- gbm(form,      
					data=data, #var.monotone=c(0,0,0,0,0,0),
					distribution=distribution, 
					n.trees=ntree,
					shrinkage=shrinkage,
					interaction.depth=interaction.depth,
					bag.fraction = bag.fraction,
					train.fraction = train.fraction,
					n.minobsinnode = n.minobsinnode, 
					cv.folds = 0,
					keep.data=TRUE, 
					verbose=verbose,
					n.cores=1) )

	if(class(btr)=="try-error") {
		browser()
	}
	list(form=form, distribution=distribution, model=gbm1)
}
