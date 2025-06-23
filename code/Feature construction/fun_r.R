# referencing relatedness value for an edge list (data frame)
# argument: edgedata -> your edge list ($V1, $V2)
# argument: relate -> relatedness table ($id1, $id2, $id12, $id21, $r)

fun_r <- function(edgedata, relate){
  if(is.data.frame(edgedata)==T){
  edgedata$V12 <- paste0(edgedata[[1]], edgedata[[2]])
  rr <- relate[match(edgedata$V12, relate$id12),]$r
  rr <- ifelse(is.na(rr)==T, relate[match(edgedata$V12, relate$id21),]$r, rr)
  return(rr)
  }
  if(is.vector(edgedata)==T){
  V12 <- paste0(edgedata[1], edgedata[2])
  rr <-  relate[match(V12, relate$id12),]$r
  rr <- ifelse(is.na(rr)==T, relate[match(V12, relate$id21),]$r, rr)
  return(rr)  
  }
}

fun_r2 <- function(edgedata, relate, vname){
  if(is.data.frame(edgedata)==T){
    p <- which(colnames(relate)==vname)
    
    edgedata$V12 <- paste0(edgedata[[1]], edgedata[[2]])
    rr <- relate[match(edgedata$V12, relate$id12),][[p]]
    rr <- ifelse(is.na(rr)==T, relate[match(edgedata$V12, relate$id21),][[p]], rr)
    return(rr)
  }
  if(is.vector(edgedata)==T){
    V12 <- paste0(edgedata[1], edgedata[2])
    rr <-  relate[match(V12, relate$id12),][[p]]
    rr <- ifelse(is.na(rr)==T, relate[match(V12, relate$id21),][[p]], rr)
    return(rr)  
  }
}



