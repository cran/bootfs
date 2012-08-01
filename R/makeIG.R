## importance graph for one single classification method.
makeIG <- function(res_bstr, SUBDIR=NULL, prob=.975) {
	if(!is.null(SUBDIR)) {
		pdf(paste(SUBDIR, "importance_graph.pdf", sep="/"), width=25, height=25)
	}
	for(strat in names(res_bstr)) {
		## get the features for each of the bstr bootrapping runs
		if(class(res_bstr[[strat]])=="try-error")
            next
            
        allsignatures <- extractsignatures(res_bstr, strat)

		## make the unique features
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
		filter <- min((max(adj2)-1),round(quantile(adj2, prob=prob))) 
		importance_igraph(adj2, main=strat, layout="layout.ellipsis", vlabel.cex=3, filter=filter)
	}
	if(!is.null(SUBDIR)) {
		dev.off()
	}
	list(allsignatures=allsignatures, allprots=allprots, adj=adj, filter=filter)
}
