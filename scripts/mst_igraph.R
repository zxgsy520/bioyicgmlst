#!/usr/bin/env Rscript

library(igraph)
options(bitmapType='cairo') #关闭服务器与界面的互动响应


cluster_group <- function(group){
    
    groupid <- unique(group$group)
    r <- list()
    n <- 0
    for (i in groupid){
        n <- n + 1
        r[paste(n, "", sep="")] <- list(which(group$group==i))
        #r <- c(r, list(which(group$group==i)))
    }

    return(r)
}


plot_mst_igraph <- function(file, prefix, groupf=FALSE){
    
    data <- read.table(file, header=T, row.names=1, sep="\t")
    samples <- colnames(data)
    samplelen <- length(samples)
  
    newdata <- matrix(1:samplelen*samplelen, samplelen, samplelen, byrow=T) #转换为矩阵
    n <- 0
    for (i in samples){
        n <- n + 1
        mat <- as.matrix(data[,i])
        newdata[,n] <- mat
    }
   
    clgroup <- ""
    if (groupf!=FALSE){
        group <- read.delim(groupf, sep="\t", stringsAsFactors=FALSE, header=TRUE)
        colnames(group) <- c("sample", "group")
        rownames(group) <- group$sample
        group <- group[samples, ]
        clgroup <- cluster_group(group)
        
    }
   
    mst <- graph.adjacency(newdata, weighted=TRUE, mode="undirected")
    mst <- minimum.spanning.tree(mst, weights=graph_attr(mst, "weight"))
    V(mst)$label <- samples

    pdf(paste(prefix, ".mst.pdf", sep=""))
    a <- dev.cur()
    png(paste(prefix, ".mst.png", sep=""), width=1600, height=1600, res=72*3)
    dev.control('enable')
    #mstcw <- cluster_walktrap(mst)
    #mstcw <- cluster_edge_betweenness(mst)
    #plot(mstcw, mst)

    if (groupf!=FALSE){
        plot(mst, edge.label=E(mst)$weight, vertex.label=samples,
             layout=layout_as_tree(mst, circular=TRUE), #layout_as_tree(mst, circular=TRUE)(局方式发散), layout=layout_in_circle(圆形布局) 
             vertex.shape="circle", #节点边框none为无方框，circle为圆形边框，rectangle为方形边框
             vertex.label.cex=1, #节点字体大小
             vertex.size=5, #节点大小 map(degree(data), c(1, 20)) 节点大小根据联系人多少设置
             vertex.label.dist=0.5,
             edge.curved=0.1,
             edge.width=1.5,
             edge.label.cex=0.8,
             edge.color="green",
             mark.groups=clgroup #添加分组
             )
    }else{
        plot(mst, edge.label=E(mst)$weight, vertex.label=samples,
             layout=layout_as_tree(mst, circular=TRUE), #layout_as_tree(mst, circular=TRUE)(局方式发散), layout=layout_in_circle(圆形布局) 
             vertex.shape="circle", #节点边框none为无方框，circle为圆形边框，rectangle为方形边框
             vertex.label.cex=1, #节点字体大小
             vertex.size=5, #节点大小 map(degree(data), c(1, 20)) 节点大小根据联系人多少设置
             vertex.label.dist=0.5,
             edge.curved=0.1,
             edge.width=1.5,
             edge.label.cex=0.8,
             edge.color="green"
             )
    }
    dev.copy(which=a)#复制png的图形给pdf
    dev.off()
    dev.off()

    V(mst)$shape <- "circle"
    V(mst)$size <- 5
    V(mst)$label.cex <- 1
    V(mst)$label.dist <- 0.5
    E(mst)$label <- E(mst)$weight
    E(mst)$width <- 1.5
    E(mst)$curved <- 0.2
    E(mst)$label.cex <- 0.8
    E(mst)$color <- "green"
    
    write_graph(mst, paste(prefix, ".mst.graphml", sep=""), "graphml")
}


add_help_args <- function(args){

    if(length(args) < 2) {
        cat("Version: v1.1.0\n")
        cat("Author:Xingguo Zhang\n")
        cat("Email:invicoun@foxmail.com\n")
        cat("Function:Plot minimum generation diagram.\n")
        cat("Example1:mst_igraph.R distances.tab prefix\n")
        cat("Example2:mst_igraph.R distances.tab prefix group.list\n")
        cat("Input distances.tab file format:\n")
        cat("Tax Id\tsample1\tsample2\tsample3\n")
        cat("sample1\t0\t9\t9")
        cat("sample2\t9\t0\t2")
        quit()
    }
}


args <- commandArgs(trailingOnly=TRUE)
add_help_args(args)

groupf <- FALSE
if(length(args) == 3) {
    groupf <- args[3]
}

plot_mst_igraph(args[1], args[2], groupf)
