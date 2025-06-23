#function calculating community robustness

fun_rcomm <- function(data0, n.bootstraps=100, plot.result=F, output=c("rcomm","MGList")){
  
  # Create space to store results from bootstraps
  network.community <- matrix(0, ncol(data0), ncol(data0))
  network.present <- matrix(0, ncol(data0), ncol(data0))
  network.QG <- matrix(1:n.bootstraps, n.bootstraps, 3)
  
  # 1. Calculate network
  network <- get_network(data0, data_format="GBI", association_index="SRI")
  
  # 2. Calculate community membership of the observed network
  community.observed <- fastgreedy.community(graph.adjacency(network, mode="undirected",weighted=TRUE))
  
  # 3. Main bootstrapping method: 
       #i) Bootstrap the observed data0
       #ii) recalculate the network, 
       #iii) recalculate community membership, iv) check if both individuals are observed
  
  for (i in 1:n.bootstraps) {
    # This step bootrstraps the sampling periods
    gbi.boot <- data0[sample(1:nrow(data0), nrow(data0), replace=TRUE),];
    sink("nul"); #這行可避免console秀出所有跑的結果
    network.boot <- get_network(gbi.boot, data_format="GBI", association_index="HWI")
    sink(); #只要有前面有用到sink後面一定要加上sink() 否則會有error
        # This step calculates the community membership from the bootstrapped network
    community.boot <- fastgreedy.community(graph.adjacency(network.boot,mode="undirected",weighted=TRUE))
    
    # This step adds 1 to any dyads in the same community
    network.community <- network.community +  outer(community.boot$membership, community.boot$membership,"==")
    
    # This step records num of group and modularity of each boot.graph
    network.QG[i, 2] <- modularity(community.boot)
    network.QG[i, 3] <- max(community.boot$membership)
    # This step adds 1 to any dyads that are both present (in this case if they have at least 1 edge)
    network.present <- network.present + outer((rowSums(network.boot) > 0), (rowSums(network.boot) > 0),"*")
  }
  # End bootstrap
  
  # outout matrix of num of group and modularity of each boot.graph (Q means modularity)
  Qlist<-network.QG
  colnames(Qlist)<-c("X","Mod","groupnum")
  if(output=="MGList"){return(Qlist)}
  
  
  # Calculate proportion of times observed in the same community
  P <- network.community/network.present
  P[!is.finite(P)] <- 0
  
  # Calculate assortment from known community membership
  require(assortnet)
  rc <- assortment.discrete(P,community.observed$membership)$r
  
  #if the argument plot.result=T, then generate plot network of probabilities that nodes are assigned to the same community in bootstraps. 
  #It will be saved as pdf file called "rc_result.pdf" in your R output folder
  if(plot.result) {
    pdf("rc_result.pdf") 
    diag(P)=0
    g = graph.adjacency(P, "undirected", weighted=T)
    plot(g, edge.width=E(g)$weight, vertex.label="", vertex.size=5, vertex.color=membership(community.observed))
    dev.off()
  }
  
  #output rcomm
  if(output=="rcomm"){return(rc)}
}
#end function
