#******Methods******#
#get % of samping interval > 2hrs
cutoff = 1
group_type = "BNB"  #B, NB, BNB
index = "SRI"
format = "gbi"

#import gmm files, 
b <- list.files(paste0("../output/data/gmm/BNB", "/v", cutoff))

#generate list with gbi and event
f <- function(b){
  a <- readRDS(paste0("../output/data/gmm/BNB", "/v", cutoff, "/", b))
  
  event <- a$metadata %>% 
    mutate(date = str_split_fixed(Location, "_", n=3)[,2], 
           nest = str_split_fixed(Location, "_", n=3)[,1]) %>% 
    mutate(date = lubridate::mdy(date), 
           year = as.character(year(date))) %>%
    mutate(duration_min = (End - Start)/60) %>% #sampling intervals 
    left_join(nest_list_uni, by = c("nest")) %>%  #add $colony
    left_join(select(colonywise_sampledate, colony, date, colony_sampled_D), by = c("colony","date")) %>% 
    left_join(select(colony_excluded, year, colony, excluded), by = c("year", "colony")) %>%
    mutate(excluded = replace_na(excluded, 0)) %>%
    setDT()
  
  #only systematic sampling and <2hrs sampling interval
  position_ss <- which(event$colony_sampled_D==1)
  
  event_select <- event[position_ss,]
  gbi <- a$gbi[position_ss, ]
  nzindex <- which(colSums(gbi)>0)
  gbi <- gbi[,nzindex]
  
  outputlist <- list("gbi" = gbi, "event" = event_select)
  return(outputlist)
}

# make a list for gmm data from all plots, years
gmm.list <- lapply(b, f)  

# add name to the list
#names(my_list) <- c(“v”, “m”, “df”)
if(group_type == "BNB"){
  v_name <- substr(b, 9, 11)
} else if(group_type == "B"){
  v_name <- substr(b, 7, 9)
} else {
  v_name <- substr(b, 8, 10)
}

names(gmm.list) <- v_name

#
f_duration <- function(data){
  d <- data$event$duration_min
} 

intervals <- lapply(gmm.list, f_duration) %>% map_df(., ~as.data.frame(.x), .id="yearplot") %>%  mutate(exclude = ifelse(.x > 120, 1, 0))

sumt <- intervals %>% count(exclude) %>% mutate(sum = sum(n), pro = n/sum)


