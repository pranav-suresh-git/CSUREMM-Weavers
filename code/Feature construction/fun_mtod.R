#Matrix to data frame
fun_mtod <- function(x){
  d <- x %>% 
    modify_if(., upper.tri(., diag = F), ~ NA) %>%  #include diagonal (F)
    reshape2::melt(., na.rm = T) %>% 
    rename(id1 = Var1, id2 = Var2, association = value) %>%
    mutate(id1 = as.character(id1), id2 = as.character(id2)) 
  return(d)
}