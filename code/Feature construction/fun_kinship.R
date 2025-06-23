# referencing relatedness value for an edge list (data frame)
# argument: edgedata -> your edge list ($V1, $V2)
# argument: relate -> relatedness table ($id1, $id2, $id12, $id21, $r)

#----fun_kinship-----
fun_kinship <- function(edgedata, kinship, vname){  #vname = variable name for kin
  
  p <- which(colnames(kinship)==vname )
  kinship[[p]] <- as.character(kinship[[p]])
  
  if(is.data.frame(edgedata)==T){
    edgedata$V12 <- paste0(edgedata[[1]], edgedata[[2]])
    kk <- kinship[match(edgedata$V12, kinship$id12)][[p]]
    kk <- ifelse(is.na(kk)==T, kinship[match(edgedata$V12, kinship$id21)][[p]], kk)
    return(kk)
  }
  if(is.vector(edgedata)==T){
    V12 <- paste0(edgedata[1], edgedata[2])
    kk <-  kinship[match(V12, kinship$id12)][[p]]
    kk <- ifelse(is.na(kk)==T, kinship[match(V12, kinship$id21)][[p]], kk)
    return(kk)  
  }
}


