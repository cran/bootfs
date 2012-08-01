#rfclass_cv <-
cv_rfclass <- 
function(X, logX, ncv=5, repeats=10, seed=123,
	maxRuns=500, avg="none", spread.estimate="none",
	colorize=FALSE, minclassN=NULL,
	ntree=1000, localImp=TRUE, rfimportance="MeanDecreaseGini",
	fs.method="rf_boruta", 
	...) {
	Y <- X[[1]]
	filename <- X[[2]]
	## should something be plotted?
	if(filename=="-")
		filename <- NULL
	nas <- which(is.na(Y))
	if(length(nas)>0) {
		x <- as.data.frame(logX[-nas,])
		yp <- Y[-nas]
	} else {
		x <- as.data.frame(logX)
		yp <- Y
	}


	## determine the type of classification
	if(length(unique(yp))==2) {
		distribution <- "bernoulli"
		#data$traing <- factor(ifelse(traing==1, 1, 0))
		#yp <- ifelse(yp==1, 1, 0)
	} else {
		distribution <- "multinomial"
	}

	## CV
	#library(ROCR)
	#ncv <- 5
	cvby <- ceiling(nrow(x)/ncv) # round up
	## initialize the result objects
	sn <- sp <- testdim <- NULL
	fitted <- labels <- list()
	fitlist <- testlist <- features <- breslist <- list()
	it <- 0
	set.seed(seed)
	for(ri in 1:repeats) {
		## find a permutation leaving stratified test/training sets
		## with regard to the class label distributions
		#folds <- select_cv_balanced(x, yp, ncv)
		#folds <- select_cv_balanced(yp, ncv)
		nottwoclasses <- TRUE
		while(nottwoclasses) {
			folds <- createFolds(yp, k=ncv, returnTrain=FALSE) ## from caret package
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
			## perform feature selection on train data

			# create a try error object to initialise the loop
			# below -> can this be done more elegantly?
			btr <- try(silent=TRUE)
			tecnt <- 0
			while(inherits(btr,"try-error")) {
				if(tecnt>0) ## omit printing the first time
					print(tecnt)
				tecnt <- tecnt+1
				if(tecnt>10)
					stop("RF Error: no data in one group")

				## use boruta? -> all relevant variable selection
				if(fs.method=="rf_boruta") {
					btr <- try(bres <- Boruta(x=train, y=factor(traing), doTrace=2, maxRuns=maxRuns))
					selprobes <- gsub("`","",names(bres$finalDecision[which(bres$finalDecision!="Rejected")]))
				} else {
					## use a default random forest: minimum necessary selection
					cat(".")
					btr <- try(bres <- randomForest(x=train, y=factor(traing),  xtest=NULL, ytest=NULL, ntree=ntree, keep.forest=TRUE, proximity=TRUE, localImp=localImp))
					imp <- importance(bres)
					ord <- order(imp[,rfimportance], decreasing=TRUE)
					imp <- imp[ord,]
					selprobes <- rownames(imp)[which(imp[,rfimportance]>=1)] ## ranked from most important to least important
					# if no probes were selected, take the first half of the features (in order)
					if(length(selprobes)==0) {
						selprobes <- rownames(imp)[1:(floor(nrow(imp)/2))]
					}
				}
			}
			## train random forest on selected features
			if(length(selprobes)>0) {
				randf <- randomForest(train[,selprobes], y=factor(traing),  xtest=NULL, ytest=NULL, ntree=500, keep.forest=TRUE, proximity=TRUE, localImp=FALSE)
			}

			## predict for multinomial or bernoulli
			if(distribution=="multinomial") {
				rfpred <- predict(randf, newdata=test, type="prob")
				Ncl <- ncol(rfpred)
				kx <- 0:Ncl * 2
				## sometimes more than one group is max. then choose randomly
				#argmax <- unlist(apply(rfpred, 1, function(x) sample(which(x==max(x)))[1]))
				## sometimes more than one group is max. then choose the first one
				argmax <- unlist(apply(rfpred, 1, function(x) which(x==max(x))[1]))
				allmax <- unlist(apply(rfpred, 1, max))
				randfpred <- kx[argmax] + allmax
				# attach the prediction probability for the classes. Note that these are one probability for each class, i.e. a matrix with n.class columns for each fold/repeat
				#fitted <- cbind(fitted, randfpred)
				fitted[[it]] <- randfpred
			} else {
				## predict using the rfrand model
				randfpred <- predict(randf, test, type="prob")
				#fitted <- cbind(fitted, randfpred[,2]) # attach the prediction probability for the positive class
				fitted[[it]] <- randfpred[,2]
			}
			#labels <- cbind(labels, testg)
			if(length(testg) != length(fitted[[it]])) {
				print("****************** OOPS: test vector not of equal length")
				browser()
			}
			labels[[it]] <- testg
			fitlist[[it]] <- randf # random forest on train dat with selected features
			testlist[[it]] <- randfpred # corresponding prediction
			features[[it]] <- selprobes
			breslist[[it]] <- bres # boruta training objec
			rm(bres)
			
		} # cv loop
	} # repeat loop

	## create ROC curves for performance evaluation
	if(!is.null(filename)) {
		pdf(filename)
	}


	## handle binomial and multiclass classification differently
	if(distribution=="multinomial") {
		aucs <- vector("numeric", length(fitted))	
		for(ki in 1:length(labels)) {
			grpx <- labels[[ki]]
			predx <- fitted[[ki]]
			aucs[ki] <- multiclass.roc(grpx~predx)$auc
		}
		## plot auc distribution
		boxplot(aucs, ylim=c(0,1), main=c(paste(Ncl, "- class classification"), "multinomial model"))
		axis(1, at=1, labels="multiclass AUC")
		legend("bottomright", border="white", fill="white", legend=c("classes:",levels(Y)))
		roc.curve <- NULL
		auc <- signif(median(aucs,na.rm=TRUE), digits=3)
	} else {
		auc <- roc(as.vector(unlist(fitted)),as.vector(unlist(labels)),measure="tpr",x.measure="fpr",colorize=colorize, avg="none", spread.estimate="none", filter=0)
		title(main="ROC curves averaged over all CV runs")	
		#auc <- roc(fitted,labels,measure="tpr",x.measure="fpr",colorize=colorize, avg=avg, spread.estimate=spread.estimate, filter=0)
		#title(main="ROC curves for each CV run")	#roc(as.vector(fitted),as.vector(labels),measure="tpr",x.measure="fpr",colorize=colorize, avg="none", spread.estimate="none", filter=0)
		#title(main="ROC curves averaged over all CV runs")
		roc_binterval(fitted, labels)
		pred <- prediction(unlist(fitted), unlist(labels))
		roc.curve <- performance(pred, measure = "tpr", x.measure = "fpr")
		aucs <- unlist(performance(pred, "auc")@y.values)
	}
	if(!is.null(filename)) {
		dev.off()
	}
	performance <- list(fitted=fitted, labels=labels, aucs=aucs, auc=auc, roc.curve=roc.curve, classes=levels(Y))
	## todo remove double saving of fitted and labels
	cvobj <- list(fitlist=fitlist, testlist=testlist, features=features, breslist=breslist)
	#cvobj <- list(fitted=fitted, labels=labels, fitlist=fitlist, testlist=testlist, features=features, breslist=breslist)

	## train on all data
	if(fs.method=="rf_boruta") {
		bres <- Boruta(x=x, y=factor(yp), doTrace=2, maxRuns=500)
		selprobes <- gsub("`","",names(bres$finalDecision[which(bres$finalDecision!="Rejected")]))
	} else {
		bres <- randomForest(x=x, y=factor(yp),  xtest=NULL, ytest=NULL, ntree=ntree, keep.forest=TRUE, proximity=TRUE, localImp=localImp)
		imp <- importance(bres)
		ord <- order(imp[,rfimportance], decreasing=TRUE)
		imp <- imp[ord,]
		selprobes <- rownames(imp)[which(imp[,rfimportance]>=1)] ## ranked from most important to least important
		# if no probes were selected, take the first half of the features (in order)
		if(length(selprobes)==0) {
			selprobes <- rownames(imp)[1:(floor(nrow(imp)/2))]
		}

	}
	## this is the final model; here of course a selection bias applies, the model 
	## not really helpful. TODO: remove this
	if(length(selprobes)>0) {
		randf <- randomForest(x[,selprobes], y=factor(yp),  xtest=NULL, ytest=NULL, ntree=500, keep.forest=TRUE, proximity=TRUE)
	}

	## boruta adds a hochkomma to names that contain '-'-characters. have to remove these
	#browser()
	#~     pdf(filename)
	#~     plot(bres)
	#~     if(length(selprobes)>0) {
	#~         MDSplot(randf,fac=factor(yp))
	#~     }
	#~     dev.off()
	err <- round(randf$err.rate[randf$ntree, "OOB"] * 100, digits = 2)
	#err <- sum(rowSums(cbind(yp, as.numeric(as.character(predict(randf, x)))))==0)*100
	list(bres=bres, randf=randf, selprobes=selprobes, err=err, cvobj=cvobj, performance=performance)
}
