#analysis of null model
library(asnipe)
library(sna)
library(dplyr)
library(tidyr)

#generate your random networks
#define # of flips = ceiling(permutations/return)
#10flips seems to have significant reduction in RME (Whitehead 2005)
#Do I need to do different flips till p value stablizes?

f <- function(gbi, nperms = 1000, flips = 50){  #npwem=1000, flips=50
  random_networks <- network_permutation(gbi, "GBI", permutations = nperms*flips, association_index = "SRI", returns = flips)  #data stream permutation
  
  # mean association index for random networks
  mean_rand <- rep(0, nperms)
  for(i in c(1:nperms)){
    mean_rand[i] <- mean(random_networks[i,,])
  }
  
  # CV association index
  cv_rand <- rep(0, nperms)
  for(i in c(1:nperms)){
    cv_rand[i] <- sd(random_networks[i, , ])/mean(random_networks[i, , ])*100
  }
  
  #observed network
  network <- asnipe::get_network(gbi, data_format = "GBI", association_index = "SRI")
  
  #stats for observed nw
  mean_obs <- mean(network, na.rm=T)
  cv_obs <- sd(network)/mean_obs*100
  
  #mean association in random networks
  meanAI_rand <- mean(mean_rand)
  
  # calculate p-vale
  p_mean <- sum(mean_rand > mean_obs)/1000 #obs would be larger if non-random
  p_cv <- sum(cv_rand > cv_obs)/1000
  
  output <- data.frame(p_mean, p_cv, meanAI_rand)
  return(output)
  
}

#daytime
#p value for all plots
pp.list <- lapply(Dgbi.pssa, f) # p value list
pp_all <- rbindlist(pp.list, idcol = "net")

#write_xlsx(pp_all, "../output/null model/output_null model table_v0.xlsx")

#breeding
ppB.list <- lapply(Bgbi.p, f) # p value list
ppB_all <- rbindlist(ppB.list, idcol = "net")

#write_xlsx(ppB_all, "../output/null model/output_null model table_B_v0.xlsx")

#p value for all colonies
p.list <- lapply(Dgbi, f) # p value list
p_all <-  do.call(rbind, p.list)



#write_xlsx(p_all, "../output/null model/output_null model p value_50flips_colony.xlsx")
#write_xlsx(pp_all, "../output/null model/output_null model p value_50flips_plot.xlsx")