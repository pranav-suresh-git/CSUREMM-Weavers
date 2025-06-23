# This is a compilation of functions for plottint networks
# fun_attr: add attributes on igraph object
# fun_plotnw: plot networks
# fun_plotcom: plot community networks
# fun_plotmg: plot multiple network graphs (e.g. interaction + relatedness)
# fun_plotnbi: plot bipartite network (bird and nest)
# fun_plotgs: plot group size distribution
# [netbird_att], [colony_member]needs to be data.table

# fun_attr----
# add attributes
fun_attr <- function(net, type){  
  V(net)$sex <- nestbird_attr[match(V(net)$name, node)]$sex  #add $sex on nodes
  V(net)$color <- ifelse(V(net)$sex == "f", "yellow", "dodgerblue")
  V(net)$shape <- ifelse(V(net)$sex == "f", "circle", "square")
  E(net)$type <- ifelse(is.null(E(net)$type), type, E(net)$type)  # add $type on the edge
  E(net)$color <- ifelse(E(net)$kinorder == "first", "#EC7AA3", "#E1BE6A") #first, second
  return(net)
}


# fun_plotnw----
# plot network, ewidth=edge filter
fun_plotnw <- function(net, netname = "", vsize = 5, esize = 5, ewidth = 0, vname = "y", vnsize = 0.7, sex = "mf"){
  if(!exists("mytriangle", mode="function")) source("fun_add_triangle.R")  #add triangle shape
 
  #name =  deparse(substitute(net))

  net <- igraph::delete.edges(net, which(E(net)$weight < ewidth))
  
  V(net)$sex <- nestbird_attr[match(V(net)$name, node)]$sex 
  V(net)$colony <- nestbird_attr[match(V(net)$name, node)]$colony
  V(net)$plot <- nestbird_attr[match(V(net)$name, node)]$plot
  
  V(net)$color <- c("m"="dodgerblue", "f"="yellow", "NA" = "brown")[V(net)$sex] 
  V(net)$shape <- c("m"="square", "f"="circle", "NA" = "triangle")[V(net)$sex]
  
  if(sex == "m"){
    net <- igraph::delete_vertices(net, which(V(net)$sex == "f"))
  }
  
  if(sex == "f"){
    net <- igraph::delete_vertices(net, which(V(net)$sex == "m"))
  }
  
  if(sex == "mf"){
    net <- net
  }
  
  set.seed(123) #assign coordinates for nodes
  l <- layout_with_fr(net) #Fruchterman-Reingold(force-directed layout)
  
  if(vname == "y"){
   g <-
     plot(net, layout = l,
         vertex.color = V(net)$color, 
         vertex.shape = V(net)$shape,
         vertex.size = vsize, 
         vertex.label = V(net)$name,
         vertex.label.cex = vnsize,  
         edge.curved = 0, 
         vertex.label.cex = 0.4, vertex.label.dist = 1,
         edge.width = E(net)$weight*esize, edge.color = "grey30", 
         main = netname)
  return(g)
  }
  if(vname == "n"){
    g <-
      plot(net, layout = l,
           vertex.color = V(net)$color, 
           vertex.shape = V(net)$shape,
           vertex.size = vsize, 
           vertex.label = "",
           edge.curved = 0, 
           vertex.label.cex = 0.4, vertex.label.dist = 2,
           edge.width = E(net)$weight*esize, edge.color = "grey30",
           main = netname)
    return(g)
  }
}

# fun_plotkin----
# plot network, ewidth=edge filter
fun_plotkin <- function(net, kinship = c("kinorder", "kinorder2"), netname = "", vsize = 5, esize = 5, vname = "y", vnsize = 0.7){
  if(!exists("mytriangle", mode="function")) source("fun_add_triangle.R")  #add triangle shape
  
  set.seed(123) #assign coordinates for nodes
  l <- layout_with_fr(net) #Fruchterman-Reingold(force-directed layout)
  
  #name =  deparse(substitute(net))
  
  V(net)$sex <- nestbird_attr[match(V(net)$name, node)]$sex 
  V(net)$colony <- nestbird_attr[match(V(net)$name, node)]$colony
  V(net)$plot <- nestbird_attr[match(V(net)$name, node)]$plot
  
  V(net)$color <- c("m"="dodgerblue", "f"="yellow", "NA" = "brown")[V(net)$sex] 
  V(net)$shape <- c("m"="square", "f"="circle", "NA" = "triangle")[V(net)$sex]
  
  if(kinship == "kinorder"){
      E(net)$color <- c("first"="#EFC8DD", "second" ="#E1BE6A")[E(net)$kinorder]
  }
  if(kinship == "kinorder2"){
      E(net)$color <- c("PO"="#EC7AA3", "FS"="#009E73", 
                        "first"="#EFC8DD", "second" ="#E1BE6A")[E(net)$kinorder2]
  }
  
  if(vname == "y"){
    g <-
      plot(net, layout = l,
           vertex.color = V(net)$color, 
           vertex.shape = V(net)$shape,
           vertex.size = vsize, 
           vertex.label = V(net)$name,
           vertex.label.cex = vnsize,  
           edge.curved = 0.1, 
           vertex.label.cex = 0.4, vertex.label.dist = 1,
           edge.width = esize, edge.color = E(net)$color, 
           main = netname)
    return(g)
  }
  if(vname == "n"){
    g <-
      plot(net, layout = l,
           vertex.color = V(net)$color, 
           vertex.shape = V(net)$shape,
           vertex.size = vsize, 
           vertex.label = "",
           edge.curved = 0.1, 
           vertex.label.cex = 0.4, vertex.label.dist = 2,
           edge.width = esize, edge.color = E(net)$color,
           main = netname)
    return(g)
  }
}

#-------------------fun_plotcom----------------------
# plot communities in the community detection 
fun_plotcom <- function(net, size, ewidth=0, vname = c("y", "n"), vnsize = 0.6){
  if(!exists("mytriangle", mode="function")) source("fun_add_triangle.R")  #add triangle shape
  net <- igraph::delete.edges(net, which(E(net)$weight < ewidth))

  netname = deparse(substitute(net))
  v_year = substr(netname, 5, 8)
  v_colony = substr(netname, 10, 16)
  
  V(net)$sex <- nestbird_attr[match(V(net)$name, node)]$sex 
  V(net)$colony <- nestbird_attr[match(V(net)$name, node)]$colony
  V(net)$samecolony <- ifelse(V(net)$name %in% colony_member[year == v_year & colony == v_colony]$color, 1, 0)
  V(net)$plot <- nestbird_attr[match(V(net)$name, node)]$plot
  V(net)$color <- ifelse(V(net)$samecolony == 1, "black", "white")
  #V(net)$color <- c("m"="dodgerblue", "f"="yellow", "NA" = "brown")[V(net)$sex] 
  V(net)$shape <- c("m"="square", "f"="circle", "NA" = "triangle")[V(net)$sex]
  
  fg <- igraph::fastgreedy.community(net, weights = E(net)$weight)
  V(net)$community <- fg$membership
  
  library(RColorBrewer)
  
  colrs <- colorRampPalette(brewer.pal(n = 9, "Set3"))(length(communities(fg)))
  #colrs <- brewer.pal(n = length(communities(fg)), name = "Set3")
  
  set.seed(123)
  layout = layout_nicely
  
  if(vname == "y"){
  g <-
    plot(net, 
         layout = layout,
         vertex.color = V(net)$color,
         vertex.shape = V(net)$shape,
         vertex.size = size,
         #vertex.size = igraph::degree(net)*size, 
         vertex.label = V(net)$name, edge.curved = 0, 
         vertex.label.cex = vnsize, vertex.label.dist = 1,
         edge.width = E(net)$weight*4, edge.color = "grey30", 
         mark.groups = communities(fg),   # mark groups
         mark.col = colrs, mark.border="black",
         main = netname)
  return(g)
  }
  if(vname == "n"){
    g <-
      plot(net, 
           layout = layout,
           vertex.color = V(net)$color,
           vertex.shape = V(net)$shape,
           vertex.size = size,
           #vertex.size = igraph::degree(net)*size, 
           vertex.label = "", edge.curved = 0, 
           vertex.label.cex = vnsize, vertex.label.dist = 1,
           edge.width = E(net)$weight*4, edge.color = "grey30", 
           mark.groups = communities(fg),   # mark groups
           mark.col = colrs, mark.border="black",
           main = netname)
    return(g)
  }
}

#IF not using net list
fun_plotcom2 <- function(net, size, ewidth=0, vname = c("y", "n"), vnsize = 0.6){
  if(!exists("mytriangle", mode="function")) source("fun_add_triangle.R")  #add triangle shape
  net <- igraph::delete.edges(net, which(E(net)$weight < ewidth))
  
  V(net)$sex <- nestbird_attr[match(V(net)$name, node)]$sex 
  V(net)$colony <- nestbird_attr[match(V(net)$name, node)]$colony
  #V(net)$samecolony <- ifelse(V(net)$name %in% colony_member[year == v_year & colony == v_colony]$color, 1, 0)
  V(net)$plot <- nestbird_attr[match(V(net)$name, node)]$plot
  #V(net)$color <- ifelse(V(net)$samecolony == 1, "black", "white")
  V(net)$color <- c("m"="dodgerblue", "f"="yellow", "NA" = "brown")[V(net)$sex] 
  V(net)$shape <- c("m"="square", "f"="circle", "NA" = "triangle")[V(net)$sex]
  
  fg <- igraph::fastgreedy.community(net, weights = E(net)$weight)
  V(net)$community <- fg$membership
  
  library(RColorBrewer)
  
  colrs <- colorRampPalette(brewer.pal(n = 9, "Set3"))(length(communities(fg)))
  #colrs <- brewer.pal(n = length(communities(fg)), name = "Set3")
  
  set.seed(123)
  layout = layout_nicely
  
  if(vname == "y"){
    g <-
      plot(net, 
           layout = layout,
           vertex.color = V(net)$color,
           vertex.shape = V(net)$shape,
           vertex.size = size,
           #vertex.size = igraph::degree(net)*size, 
           vertex.label = V(net)$name, edge.curved = 0, 
           vertex.label.cex = vnsize, vertex.label.dist = 1,
           edge.width = E(net)$weight*4, edge.color = "grey30", 
           mark.groups = communities(fg),   # mark groups
           mark.col = colrs, mark.border="black")
    return(g)
  }
  if(vname == "n"){
    g <-
      plot(net, 
           layout = layout,
           vertex.color = V(net)$color,
           vertex.shape = V(net)$shape,
           vertex.size = size,
           #vertex.size = igraph::degree(net)*size, 
           vertex.label = "", edge.curved = 0, 
           vertex.label.cex = vnsize, vertex.label.dist = 1,
           edge.width = E(net)$weight*4, edge.color = "grey30", 
           mark.groups = communities(fg),   # mark groups
           mark.col = colrs, mark.border="black")
    return(g)
  }
}

#-----------------fun_plotcom_hc-------------------
fun_plotcom_hc <- function(net, membership, size=3, ewidth=0, vname = c("y", "n"), vnsize = 0.6){
  net <- igraph::delete.edges(net, which(E(net)$weight < ewidth))
  
  V(net)$sex <- nestbird_attr[match(V(net)$name, node)]$sex 
  V(net)$color <- c("m"="dodgerblue", "f"="yellow")[V(net)$sex] 
  V(net)$shape <- c("m"="square", "f"="circle")[V(net)$sex]
  
  library(RColorBrewer)
  
  colrs <- colorRampPalette(brewer.pal(n = 9, "Set3"))(length(membership))
  #colrs <- brewer.pal(n = length(communities(fg)), name = "Set3")
  
  set.seed(123)
  layout = layout_nicely
  
  if(vname == "y"){
    g <-
      plot(net, 
           layout = layout,
           vertex.color = V(net)$color,
           vertex.shape = V(net)$shape,
           vertex.size = size,
           vertex.label = V(net)$name, edge.curved = 0, 
           vertex.label.cex = vnsize, vertex.label.dist = 1,
           edge.width = E(net)$weight*4, edge.color = "grey30", 
           mark.groups = membership,   # mark groups
           mark.col = colrs, mark.border="black")
    return(g)
  }
  if(vname == "n"){
    g <-
      plot(net, 
           layout = layout,
           vertex.color = V(net)$color,
           vertex.shape = V(net)$shape,
           vertex.size = size,
           #vertex.size = igraph::degree(net)*size, 
           vertex.label = "", edge.curved = 0, 
           vertex.label.cex = vnsize, vertex.label.dist = 1,
           edge.width = E(net)$weight*4, edge.color = "grey30", 
           mark.groups = membership,   # mark groups
           mark.col = colrs, mark.border="black",
           main = netname)
    return(g)
  }
}


# -----------------fun_plotmg-------------------------
#plot multiple layers networks
fun_plotmg <- function(net){
  E(net)$color <- ifelse(E(net)$type == "interaction", "blue", "red")
  l <- layout_with_fr(net)
  plot(net, layout = l,
       vertex.size = 7, vertex.color = V(net)$color, vertex.shape = V(net)$shape,
       vertex.label.cex = .5, vertex.label = V(net)$name, vertex.label.dist = 2, 
       edge.color = E(net)$color, edge.curved = 0.5, edge.width = E(net)$weight, 
       main = deparse(substitute(net)))
}


# fun_plotnbi----
fun_plotnbi <- function(netnbi){
  if(!exists("mytriangle", mode="function")) source("fun_add_triangle.R")  #add triangle shape
  
  V(netnbi)$sex <- nestbird_attr[match(V(netnbi)$name, node)]$sex 
  V(netnbi)$colony <- nestbird_attr[match(V(netnbi)$name, node)]$colony
  V(netnbi)$plot <- nestbird_attr[match(V(netnbi)$name, node)]$plot
  
  V(netnbi)$color <- c("m"="dodgerblue", "f"="yellow", "NA" = "brown")[V(netnbi)$sex] 
  V(netnbi)$shape <- c("m"="square", "f"="circle", "NA" = "triangle")[V(netnbi)$sex]
  
  l <- layout_with_fr(netnbi) #Fruchterman-Reingold(force-directed layout)
  
  plot(netnbi, layout = l,
       vertex.size = 7, 
       vertex.label = V(netnbi)$name, 
       vertex.label.cex = 0.7, vertex.label.dist = 1, vertex.shape = V(netnbi)$shape,
       edge.curved = 0, edge.width = E(netnbi)$weight, edge.color = "grey30" 
  )
  #mark.groups = V(netnbi)$colony,   # mark groups
  #mark.col = NULL, mark.border="black")  
}

#function for plotting group size distribution
fun_plotgs <- function(data){
  sum_gs <- data %>% count(group) %>% rename(gs = n) %>% count(gs)
  ggplot(sum_gs, aes(x = gs, y = n)) +
    theme_classic() +
    geom_bar(fill = "grey", stat = "identity") +
    scale_x_continuous(breaks = seq(0, nrow(sum), 1)) +
    labs(x = "group size", title = "test") +
    #scale_y_continuous(limits = c(0,60))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title.x = element_text(face="bold", size=14),
          axis.title.y = element_text(face="bold", size=14))
}

