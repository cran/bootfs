
makeIG <- function(res_bstr, SUBDIR=NULL) {
	if(!is.null(SUBDIR)) {
		pdf(paste(SUBDIR, "importance_graph.pdf", sep="/"), width=25, height=25)
	}
	for(strat in names(res_bstr)) {
		## get the features for each of the bstr bootrapping runs
		if(class(res_bstr[[strat]])=="try-error")
            next
        ## find out which algorithm was applied
        if ("selected_names" %in% names(res_bstr[[1]][[1]])) { # PAMR
        	allsignatures <- sapply(res_bstr[[strat]], function(pamo) pamo$selected_names)
        } else if ("model" %in% names(res_bstr[[1]][[1]])) { # SCAD
			allsignatures <- sapply(res_bstr[[strat]], function(scad) names(scad$model$fit.info$model.list$model$w), simplify=FALSE)
        } else if ("selprobes" %in% names(res_bstr[[1]][[1]])) { # RF BORUTA
			allsignatures <- sapply(res_bstr[[strat]], function(rfso) rfso$selprobes)
        }
		## unique features
		allprots <- unique(unlist(allsignatures))
		## count (co)occurences
		adj <- matrix(0, nrow=length(allprots), ncol=length(allprots), dimnames=list(allprots, allprots))
		for(i in 1:length(allsignatures)) {
			signat <- allsignatures[[i]]
			## increment edge counter for each pairwise connection
			adj[signat,signat] <- adj[signat,signat] + 1
		}
		## order
		ord <- order(rownames(adj))
		adj2 <- adj[ord,ord]
		filter <- min((max(adj2)-1),round(quantile(adj2, prob=.9))) # only show the 10%  most frequent occuring edges
		#importance_igraph(detailed.to.simple.regulations(adj2), weights=adj2, main=strat, layout="layout.ellipsis", vlabel.cex=3, filter=filter)
		importance_igraph(adj2, main=strat, layout="layout.ellipsis", vlabel.cex=3, filter=filter)
	}
	if(!is.null(SUBDIR)) {
		dev.off()
	}
	list(allsignatures=allsignatures, allprots=allprots, adj=adj)
}
