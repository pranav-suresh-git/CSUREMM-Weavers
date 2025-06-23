# referencing sex 
# two functions: fun_sex, fun_sexdyad
# argument: edgedata -> your edge list ($V1, $V2)
# argument: nestbird_attr -> sex database(column1: combo, column2:sex)

#find sex 
fun_sex <- function(x, sex){
  s <-  sex$sex[match(x, sex[[1]])]  
  return(s)
}


# function to find sex-sex type "mm", "ff", "mf"
#output data frame, column1:V1sex, column2:V2sex, column3: V12sex
fun_sexdyad <- function(edgedata, nestbird_attr){
  if(is.data.frame(edgedata)==T){
    V1sex <-  nestbird_attr[match(edgedata[[1]], nestbird_attr[[1]])]$sex  
    V2sex <-  nestbird_attr[match(edgedata[[2]], nestbird_attr[[1]])]$sex 
    output <- data.frame(V1sex = V1sex, V2sex = V2sex) %>% 
      mutate(V12sex = paste0(V1sex, V2sex)) %>% 
      mutate(V12sex = replace(V12sex, V12sex=="fm", "mf")) %>% 
      mutate(V12sex = factor(V12sex, levels = c("mf", "ff", "mm")))
    return(output)
  }
  if(is.vector(edgedata)==T){
    V1sex <-  nestbird_attr[match(edgedata[1], nestbird_attr[[1]])]$sex  
    V2sex <-  nestbird_attr[match(edgedata[2], nestbird_attr[[1]])]$sex 
    output <- c(V1sex, V2sex, paste0(V1sex, V2sex)) 
    return(output)
  }
}





