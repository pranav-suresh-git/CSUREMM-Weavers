#OUTPUT PLOT LEVEL DYADIC ASSOCIATION

##function to convert "association matrix" to "df" and add $sex and $kinship
f_mtod <- function(x){
  d <- x %>% 
    modify_if(., upper.tri(., diag = F), ~ NA) %>%  #include diagonal (F)
    reshape2::melt(., na.rm = T) %>% 
    rename(id1 = Var1, id2 = Var2, association = value) %>%
    mutate(id1 = as.character(id1), id2 = as.character(id2)) 
  return(d)
}

## convert assocation matrix to dataframe
##this includes all combo for the plot
df_D <- lapply(Dadjm.pssa, f_mtod) %>% rbindlist(., idcol = "list") %>% mutate(gtype="daytime")
df_N <- lapply(Nadjm.pssa, f_mtod) %>% rbindlist(., idcol = "list") %>% mutate(gtype="sleeping")
df_B <- lapply(Badjm.p, f_mtod) %>% rbindlist(., idcol = "list") %>% mutate(gtype="breeding")

#[df0]----
#contain diagonal
#this contains assocations year X plot X gtype
df0 <- rbind(df_D, df_N, df_B) %>% 
  filter(id1 != "MYPO" & id2 != "MYPO") %>% #delete ind with no genetic data
  mutate(id12 = paste0(id1, id2)) %>% 
  mutate(year = paste0("20", substr(list, 6, 7)), 
         plot = if_else(str_detect(list, "M"), "MSTO", 
                        if_else(str_detect(list, "L"), "LLOD", "SPRA"))) %>% 
  select(-list) %>% 
  mutate(V1sex = fun_sexdyad(., sex)[[1]], 
         V2sex = fun_sexdyad(., sex)[[2]],
         V12sex = fun_sexdyad(., sex)[[3]]) %>% 
  mutate(r = fun_r(., relate[, r:= wang])) %>% #add $r
  mutate(r = ifelse(id1==id2, 1, r)) %>% 
  mutate(kin_final = fun_kinship(., kinship, "kin_final"),  #get $kinship 
         kin_kg = fun_kinship(., kinship, "kin_kg"), 
         kin_kg2 = fun_kinship(., kinship, "kin_kg2"),
         kinorder = fun_kinship(., kinship, "kinorder"), 
         kinorder2 = fun_kinship(., kinship, "kinorder2"),
         UR = fun_kinship(., kinship, "UR12")) %>% 
  left_join(select(pair, year, plot, V1, V2, pair), by=c("year", "plot","id1"="V1", "id2"="V2")) %>%   #add pair, one id could have two pairs!
  mutate(pair = replace(pair, is.na(pair), 0))


outdf0 <- df0 %>% filter(id1!=id2) #delete data at diagonal

#write.csv(outdf0, paste0("../output/r and association/data/output_2013-2017 weaver r and association_SS_all_yearplot", ".csv"), row.names=FALSE)