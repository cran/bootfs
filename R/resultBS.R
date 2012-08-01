resultBS <- function(results, DIR=".", vlabel.cex=3, filter=10, saveres=TRUE) {
	## combine all signatures
	if(saveres) {
		pdf(paste(DIR, "importance_graph_combined.pdf", sep="/"), width=25, height=25)
	}
	strat <- names(results[[1]])[1]
	bstr <- length(results[[1]][[1]])
 	if(any(sapply(results, function(x, strat) class(x[[strat]]), strat=strat)=="try-error")) {
		stop("Some bootstrapping runs contain errors. Remove from the result object or rerun the bootstrapping.")
	}
    allsigs <- list() 
       if("rf_boruta" %in% names(results))
		allsigs[["rf"]] <- sapply(results[["rf_boruta"]][[strat]], function(rfso) rfso$selprobes)
	if("pamr" %in% names(results))
		allsigs[["pam"]] <- sapply(results[["pamr"]][[strat]], function(pamo) pamo$selected_names)
	if("scad" %in% names(results))
		allsigs[["scad"]] <- sapply(results[["scad"]][[strat]], function(scad) names(scad$model$fit.info$model.list$model$w), simplify=FALSE)

    allsignatures <- list()
    for(i in 1:bstr) {
        allsignatures[[i]] <- intersect(intersect(allsigs[["scad"]][[i]], allsigs[["pam"]][[i]]), allsigs[["rf"]][[i]])
    }

    allprots <- unique(unlist(allsignatures))
    adj <- matrix(0, nrow=length(allprots), ncol=length(allprots), dimnames=list(allprots, allprots))
    for(i in 1:length(allsignatures)) {
        signat <- allsignatures[[i]]
        ## increment edge counter for each pairwise connection
        adj[signat,signat] <- adj[signat,signat] + 1
    }
    ord <- order(rownames(adj))
    adj2 <- adj[ord,ord]
    #importance_igraph(detailed.to.simple.regulations(adj2), weights=adj2, main=strat, layout="layout.ellipsis", vlabel.cex=3, filter=filter, ewprop=3)
    importance_igraph(adj2, main=strat, layout="layout.ellipsis", vlabel.cex=3, filter=filter, ewprop=3)
 	oo <- order(diag(adj2),decreasing=TRUE)
	adj2 <- adj2[oo,oo]
	tophits <- cbind(names(diag(adj2)), diag(adj2))
	#gene.symbol <- uc2sym(rownames(tophits)) 
	#tophits <- cbind(tophits, gene.symbol)
	if(saveres) {
		dev.off()
		write.csv2(adj2, file=paste(DIR, "adj_sorted.csv", sep="/"))
		write.csv2(tophits, file=paste(DIR, "tophits.csv", sep="/"))
	}
	
	list(allsignatures=allsignatures, allprots=allprots, adj=adj2, tophits=tophits)
}
