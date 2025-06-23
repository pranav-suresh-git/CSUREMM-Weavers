#Dyad survival
#------------repair rate--------------------


#[repair]----
repair <- pair_mf %>% 
  rename(m = V1, f = V2) %>% 
  mutate(id12 = paste0(m, f)) %>% 
  select(year, plot, m, f, id12) %>% 
  mutate(period_t1 = as.character(as.numeric(year) + 1)) %>% 
  #are they pairs in the following year?
  left_join(pair[,.(V1, V2, year, pair)], 
            by = c("period_t1"="year", "m"="V1", "f"="V2")) %>% 
  mutate(pair = ifelse(period_t1!="2018", replace_na(pair, 0), pair)) %>% 
  rename(pair_t1 = pair) %>% #$pair_t1
  #are they present during the year
  left_join(survival[,!c("year_nstl", "sex")], 
            by = c("m"="color", "period_t1" = "year")) %>% 
  rename(m_present = present) %>% 
  left_join(survival[,!c("year_nstl", "sex")], 
            by = c("f"="color", "period_t1" = "year")) %>% 
  rename(f_present = present) %>% 
  filter(pair_t1!="2018")

#do they repair in the next year?
#pairs only when both present in the year
repair_p <- repair %>%  #n=126
  filter(m_present==1 & f_present==1) 

xtabs(~ pair_t1, data = repair_p)

length(unique(repair_p$id12)) #n=102 unique pairs

