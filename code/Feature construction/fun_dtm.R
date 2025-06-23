#convert data to matrix
fun_dtm <- function(data, id1, id2, var){
  x <- data %>% 
    dplyr::select(id1, id2, var) %>% 
    spread(key = id2, value = var) %>% 
    column_to_rownames(., var = id1) %>% 
    as.matrix()
  return(x)
}