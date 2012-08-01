## TODO: clean up this function
importance_igraph <- function (phi, main = "", 
        highlight = NULL,layout="layout.ellipsis",
		pdf=NULL, pointsize=12, tk=FALSE,
		## provide an intuitive way of filtering
		node.filter=NULL, ## show all nodes by default
		edge.filter=10, ## only edges with more than 10 occurences
		node.color="grey", edge.color="#000000AA",
		vlabel.cex=0.6, vlabel.cex.min=0.5, vlabel.cex.max=4, max_node_cex=8,
        edge.width=1, max_edge_cex=6, ewprop=3 )
{
	#stopifnot(require(igraph0))
	#edge.color="#000000AA" #"black"
	edge.lty="solid"
	# store the original weights/adjacency matrix entries
	weights <- phi
	# make a qualitative matrix, holding the edge yes or no information
	phix <- ifelse(phi==0, 0, 1)
	# apply the node filter
	phi.bck <- phi
	dg <- diag(weights)
	nremove <- which(dg<node.filter)
	if(length(nremove)>0) {
		phix[nremove,] <- 0
		phix[,nremove] <- 0
		dg[nremove] <- 0
	}

	# apply the edge filter
	phix[weights<edge.filter] <- 0

	
	weights[phix==0] <- 0 ## remove edge weights where no edge should be plotted
	diag(phix) <- 0 ## do not show the self edges
	diag(weights) <- dg ## but save the information
	
	## create the igraph object
	ig <- graph.adjacency(phix)
	ig.nodes <- as.matrix(print.igraph.vs(V(ig))) 
	if(tk) {
		tkplot(ig, vertex.label=V(ig)$name)
		return(ig)
	}
	vertex.color <- rep(node.color, length(ig.nodes))
	if(!is.null(highlight)) {
		if(class(highlight)=="list") {
			highlight <- unique(unlist(highlight))
		}
		if(class(highlight)=="numeric") {
			snodes <- colnames(phi)[highlight]
		} else {
			snodes <- highlight
		}
		vertex.color[match(snodes, ig.nodes)] <- "red"
	}
	if(!all(phix==0)) {
		ig.edges <- gsub(" ","",as.matrix(print.igraph.es(E(ig))))
		edge.width <- rep(edge.width, length(ig.edges))
		if(!is.null(weights)) {
            ## get edge width proportional to weights
            epairs <- strsplit(ig.edges, "->")
            for(epi in 1:length(epairs)) {
                epair <- epairs[[epi]]
                edge.width[epi] <- weights[epair[1], epair[2]]
            }
            
		}
		
		## attributes
		print("Setting graph attributes...")
		E(ig)$color <- edge.color
		E(ig)$lty <- edge.lty
        edge.width <- edge.width^ewprop/max(edge.width^ewprop) * max_edge_cex
        edge.width[is.na(edge.width)] <- 1
		E(ig)$width <- edge.width
		E(ig)$arrow.size <- 0 # dont show the arrows
	}
	V(ig)$color <- vertex.color
    V(ig)$label.color <- "red"
    V(ig)$shape <- rep("circle",length(ig.nodes))
    nsize <- diag(weights)
    #if(!is.null(node.filter)) {
	#	nsize[nsize<=node.filter] <- 0
    #}
    nsize <- (nsize-min(nsize))/max((nsize-min(nsize)))*max_node_cex
    if(all(is.na(nsize))) 
        vlabel.cex <- 1.5
    
    if(any(is.na(nsize))) {
		nsize[is.na(nsize)] <- 0
	}
    vlabel.cex <- nsize/max(nsize) * vlabel.cex
    vlabel.cex[vlabel.cex==0] <- vlabel.cex.min
    vlabel.cex <- pmin(vlabel.cex, vlabel.cex.max)
	## if node filter was applied, make the filtered nodes very small
	if(length(nremove)>0)
		vlabel.cex[nremove] <- 0.1

    V(ig)$label.cex <- vlabel.cex
    V(ig)$size <- nsize
	print("..and plot.")
	if(!is.null(pdf))
		pdf(pdf,pointsize=pointsize)
    if(class(layout)!="function") {
        if(layout=="layout.ellipsis") {
            lc <- layout.ellipsis(ig, a=1, b=1.5)
            plot(ig, vertex.label=V(ig)$name, layout = lc, main=main, rescale=FALSE, ylim=range(lc[,2], xlim=range(lc[,1])))#, vertex.label.family="mono", edge.label.family="mono")
        }
    } else {
        plot(ig, vertex.label=V(ig)$name, layout = layout, main=main, rescale=TRUE)
    }
	if(!is.null(pdf))
		dev.off()
	invisible(list(ig=ig, layout=layout))
}

#~ 
#~ 
#~ importance_igraph <- function (phi, main = "", 
#~         highlight = NULL,	layout="layout.ellipsis",
#~ 		pdf=NULL, pointsize=12, tk=FALSE,
#~ 		node.color="grey", node.filter=NULL,
#~ 		vlabel.cex=0.6, vlabel.cex.min=0.5, vlabel.cex.max=4,
#~ 		max_node_cex=8,
#~         edge.width=1, filter=10, max_edge_cex=6, ewprop=3 )
#~ {
#~ 	#stopifnot(require(igraph0))
#~ 	edge.color="#000000AA" #"black"
#~ 	edge.lty="solid"
#~ 	weights <- phi
#~ 	phix <- ifelse(phi==0, 0, 1)
#~ 
#~     ## prune the connections with number of occurrences
#~     ## smaller than filter
#~     dw <- diag(weights)
#~     phix[weights<=filter] <- 0
#~     weights[weights<=filter] <- 0
#~     diag(phix) <- 0
#~     diag(weights) <- dw
#~ 
#~ 	ig <- graph.adjacency(phix)
#~ 	ig.nodes <- as.matrix(print.igraph.vs(V(ig))) 
#~ 	if(tk) {
#~ 		tkplot(ig, vertex.label=V(ig)$name)
#~ 		return(ig)
#~ 	}
#~ 	vertex.color <- rep(node.color, length(ig.nodes))
#~ 	if(!is.null(highlight)) {
#~ 		if(class(highlight)=="list") {
#~ 			highlight <- unique(unlist(highlight))
#~ 		}
#~ 		if(class(highlight)=="numeric") {
#~ 			snodes <- colnames(phi)[highlight]
#~ 		} else {
#~ 			snodes <- highlight
#~ 		}
#~ 		vertex.color[match(snodes, ig.nodes)] <- "red"
#~ 	}
#~ 	if(!all(phix==0)) {
#~ 		ig.edges <- gsub(" ","",as.matrix(print.igraph.es(E(ig))))
#~ 		edge.width <- rep(edge.width, length(ig.edges))
#~ 		if(!is.null(weights)) {
#~             ## get edge width proportional to weights
#~             epairs <- strsplit(ig.edges, "->")
#~             for(epi in 1:length(epairs)) {
#~                 epair <- epairs[[epi]]
#~                 edge.width[epi] <- weights[epair[1], epair[2]]
#~             }
#~             
#~ 		}
#~ 		
#~ 		## attributes
#~ 		print("Setting graph attributes...")
#~ 		E(ig)$color <- edge.color
#~ 		E(ig)$lty <- edge.lty
#~         edge.width <- edge.width^ewprop/max(edge.width^ewprop) * max_edge_cex
#~         edge.width[is.na(edge.width)] <- 1
#~ 		E(ig)$width <- edge.width
#~ 		E(ig)$arrow.size <- 0 # dont show the arrows
#~ 	}
#~ 	V(ig)$color <- vertex.color
#~     V(ig)$label.color <- "red"
#~     V(ig)$shape <- rep("circle",length(ig.nodes))
#~     nsize <- diag(weights)
#~     if(!is.null(node.filter)) {
#~ 		nsize[nsize<=node.filter] <- 0
#~     }
#~     nsize <- (nsize-min(nsize))/max((nsize-min(nsize)))*max_node_cex
#~     if(all(is.na(nsize))) 
#~         vlabel.cex <- 1.5
#~     
#~     if(any(is.na(nsize))) {
#~ 		nsize[is.na(nsize)] <- 0
#~ 	}
#~     vlabel.cex <- nsize/max(nsize) * vlabel.cex
#~     vlabel.cex[vlabel.cex==0] <- vlabel.cex.min
#~     vlabel.cex <- pmin(vlabel.cex, vlabel.cex.max)
#~     V(ig)$label.cex <- vlabel.cex
#~     V(ig)$size <- nsize
#~ 	print("..and plot.")
#~ 	if(!is.null(pdf))
#~ 		pdf(pdf,pointsize=pointsize)
#~     if(class(layout)!="function") {
#~         if(layout=="layout.ellipsis") {
#~             lc <- layout.ellipsis(ig, a=1, b=1.5)
#~             plot(ig, vertex.label=V(ig)$name, layout = lc, main=main, rescale=FALSE, ylim=range(lc[,2], xlim=range(lc[,1])))#, vertex.label.family="mono", edge.label.family="mono")
#~         }
#~     } else {
#~         plot(ig, vertex.label=V(ig)$name, layout = layout, main=main, rescale=TRUE)
#~     }
#~ 	if(!is.null(pdf))
#~ 		dev.off()
#~ 	invisible(list(ig=ig, layout=layout))
#~ }
