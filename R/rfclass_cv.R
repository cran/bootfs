rfclass_cv <-
function(X, logX, ncv=5, repeats=10, seed=123,
					maxRuns=500, avg="none", spread.estimate="none",
					colorize=FALSE, minclassN=NULL, ...) {
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
		sn <- sp <- fitted <- labels <- testdim <- NULL
		fitlist <- testlist <- features <- breslist <- list()
		it <- 0
		set.seed(seed)
		for(ri in 1:repeats) {
			## find a permutation leaving stratified test/training sets
			## with regard to the class label distributions
			folds <- select_cv_balanced(x, yp, ncv)
			for(i in 1:ncv) {
				it <- it + 1
				sel <- folds[[1]]
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
					print(tecnt)
					tecnt <- tecnt+1
					if(tecnt>10)
						stop("RF Error: no data in one group")
					btr <- try(bres <- Boruta(x=train, y=factor(traing), doTrace=2, maxRuns=maxRuns))
				}
				## train random forest on selected features
				selprobes <- gsub("`","",names(bres$finalDecision[which(bres$finalDecision!="Rejected")]))
				if(length(selprobes)>0) {
					randf <- randomForest(train[,selprobes], y=factor(traing),  xtest=NULL, ytest=NULL, ntree=500, keep.forest=TRUE, proximity=TRUE)
				}
				## predict using the rfrand model
				randfpred <- predict(randf, test, type="prob")
				fitted <- cbind(fitted, randfpred[,2]) # attach the prediction probability for the positive class
				labels <- cbind(labels, testg)
				fitlist[[it]] <- randf
				testlist[[it]] <- randfpred
				features[[it]] <- selprobes
				breslist[[it]] <- bres
				rm(bres)
			} # cv loop
		} # repeat loop

		## create ROC curves for performance evaluation
		if(!is.null(filename)) {
			pdf(filename)
		}
		auc <- roc(fitted,labels,measure="tpr",x.measure="fpr",colorize=colorize, avg=avg, spread.estimate=spread.estimate, filter=0)
		title(main="ROC curves for each CV run")
		roc(as.vector(fitted),as.vector(labels),measure="tpr",x.measure="fpr",colorize=colorize, avg="none", spread.estimate="none", filter=0)
		title(main="ROC curves averaged over all CV runs")

		if(!is.null(filename)) {
			dev.off()
		}

		cvobj <- list(fitted=fitted, labels=labels, fitlist=fitlist, testlist=testlist, features=features, breslist=breslist)


		## train on all data
		bres <- Boruta(x=x, y=factor(yp), doTrace=2, maxRuns=500)
		selprobes <- gsub("`","",names(bres$finalDecision[which(bres$finalDecision!="Rejected")]))
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
		list(bres=bres, randf=randf, selprobes=selprobes, err=err, cvobj=cvobj)
	}
