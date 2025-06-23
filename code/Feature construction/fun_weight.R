# referencing edge weight value for an edge list (data frame)
# argument: edgedata -> your edge list ($V1, $V2)
# argument: refdata --> your reference data(first and second columns should be dyads's id)

#----fun_kinship-----
fun_weight <- function(edgedata, refdata, vname){  #vname = variable name for the weight
  
  p <- which(colnames(refdata)==vname) #position of your variable
  #refdata[[p]] <- as.character(refdata[[p]])
  refdata$id12 <- paste0(refdata[[1]], refdata[[2]])
  refdata$id21 <- paste0(refdata[[2]], refdata[[1]])
  
  if(is.data.frame(edgedata)==T){
    edgedata$V12 <- paste0(edgedata[[1]], edgedata[[2]])
    kk <- refdata[match(edgedata$V12, refdata$id12), ][[p]]
    kk <- ifelse(is.na(kk)==T, refdata[match(edgedata$V12, refdata$id21), ][[p]], kk)
    return(kk)
  }
  if(is.vector(edgedata)==T){
    V12 <- paste0(edgedata[1], edgedata[2])
    kk <-  refdata[match(V12, refdata$id12), ][[p]]
    kk <- ifelse(is.na(kk)==T, refdata[match(V12, refdata$id21), ][[p]], kk)
    return(kk)  
  }
}


