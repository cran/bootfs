select_bootstrap_data <-
function(datX, datY) {
		tdY <- table(datY)
		a <- sample(which(datY==names(tdY)[1]), tdY[1], replace=TRUE)
		b <- sample(which(datY==names(tdY)[2]), tdY[2], replace=TRUE)
		dbstr <- datX[c(a,b),]
		rownames(dbstr) <- paste(rownames(dbstr), 1:nrow(dbstr), sep=".")
		dYbstr <- datY[c(a,b)]
		stopifnot(all(dim(datX)==dim(dbstr)))
		list(datX=as.matrix(dbstr), datY=as.numeric(dYbstr))
	}
