## get frequencies of features and importance graph for combination of methods
resultBS <- function(results, DIR=NULL, vlabel.cex=3, filter=10, useresults=1:length(results), plot=TRUE) {

	## find out how many different groups were analysed with the FS
	N.groups <- unique(sapply(results, function(x) length(x)))
	names.groups <- as.vector(unique(t(sapply(results, function(x) names(x)))))
	print(paste("Found the following classfication groups:", paste(names.groups, collapse=", ")))

	## save output to file
	saveres <- !is.null(DIR)
	
	allresults <- results
	ret <- list()

	for(i.gr in 1:N.groups) {
		grp <- names.groups[i.gr]
		print(paste("Processing", grp, "..."))
		if(!is.null(DIR)) {
			SUBDIR <- paste(DIR, grp, sep="/")
			if(!file.exists(SUBDIR))
				dir.create(SUBDIR)
		}
			
		results <- list()
		for(i.classif in 1:length(allresults)) {
			method <- names(allresults)[i.classif]
			xt <- allresults[[method]][grp]
			attr(xt, "fs.method") <- method
			results[[method]] <- xt
		}
	

		## combine all signatures
		strat <- names(results[[1]])[1]
		bstr <- length(results[[1]][[1]])
		if(any(sapply(results, function(x, strat) class(x[[strat]]), strat=strat)=="try-error")) {
			stop("Some bootstrapping runs contain errors. Remove from the result object or rerun the bootstrapping.")
		}
		allsigs <- list() 
		## specify which results to use
		touse <- names(results)[useresults]
		for(tu in touse) {
			allsigs[[tu]] <- extractsignatures(results[[tu]], strat)
		}
		

		## extract feature sets for each bootstrapping run
		## every run is summarized by the intersection of all methods
		allsignatures <- list() #vector("list", bstr)
		gotfeat <- NULL
		for(i in 1:bstr) {
			for(j in 1:length(allsigs)) {
			#for(j in useresults) {
				if(j==1) {
					as <- allsigs[[j]][[i]]
				} else {
					as <- intersect(as, allsigs[[j]][[i]])
				}
			}
			asn <- paste("bstr",i,sep="")
			if(is.null(as) | length(as)==0) {
				as <- ""
			} else {
				gotfeat <- c(gotfeat, i)
			}
			allsignatures[[asn]] <- as
		}
		

		allprots <- unique(unlist(allsignatures))
		if(!is.null(gotfeat)) {
			allprots <- setdiff(allprots, "")
			adj <- matrix(0, nrow=length(allprots), ncol=length(allprots), dimnames=list(allprots, allprots))
			#for(i in 1:length(allsignatures)) {
			for(i in gotfeat) {
				signat <- allsignatures[[i]]
				## increment edge counter for each pairwise connection
				adj[signat,signat] <- adj[signat,signat] + 1
			}
			ord <- order(rownames(adj))
			adj2 <- adj[ord,ord]
			
			if(saveres) {
				pdf(paste(SUBDIR, "importance_graph_combined.pdf", sep="/"), width=25, height=25)
			}
				#importance_igraph(detailed.to.simple.regulations(adj2), weights=adj2, 
				#main=strat, layout="layout.ellipsis", vlabel.cex=3, filter=filter, ewprop=3)

			if(plot) {
				importance_igraph(adj2, main=strat, layout="layout.ellipsis", vlabel.cex=vlabel.cex, edge.filter=filter, node.filter=filter, ewprop=3)
			}
			
			adjret <- adj2
			oo <- order(diag(adj2),decreasing=TRUE)
			adj2 <- adj2[oo,oo]
			tophits <- cbind(names(diag(adj2)), diag(adj2))
			#gene.symbol <- uc2sym(rownames(tophits)) 
			#tophits <- cbind(tophits, gene.symbol)
			if(saveres) {
				dev.off()
				write.csv2(adj2, file=paste(SUBDIR, "adj_sorted.csv", sep="/"))
				write.csv2(tophits, file=paste(SUBDIR, "tophits.csv", sep="/"))
			}
		} else {
			adjret <- NULL
			tophits <- ""
		}
		
		ret[[grp]] <- list(allsignatures=allsignatures, allprots=allprots, adj=adjret, tophits=tophits)
	}
	ret
}
