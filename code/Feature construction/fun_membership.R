#------functino for community detection-------#
fun_membership <- function(net, method = "fg", outfile = c("community", "member")){ #outfile can be "community" or "member"
  netname = deparse(substitute(net))
  if(method == "eb"){
    eb <- igraph::edge.betweenness.community(net, directed = F, weights = E(net)$weight)
    
    # output group membership [member_eb]----
    member_eb <- 
      data.frame(eb$names, eb$membership) %>% 
      `colnames<-`(c("id", "group")) %>% 
      mutate(net = netname) %>% 
      arrange(group)
    
  
    if(outfile == "member"){
      return(member_eb)
    }
    if(outfile == "community"){
      return(eb)
    }
  }
  if(method == "fg"){
    # fast greedy----
    fg <- igraph::fastgreedy.community(net, weights = E(net)$weight)
    
    # output group membership to a data frame
    member_fg <- 
      data.frame(fg$names, fg$membership) %>% 
      `colnames<-`(c("id", "group")) %>% 
      mutate(net = netname) %>% 
      arrange(group)

    
    if(outfile == "member"){
      return(member_fg)
    }
    if(outfile == "community"){
      return(fg)
    }
  }
}
