#Chap2 coding: selected from analysis_dynamics2
#Run Chap2_import and tidy data.R first


#---------------COLONY DYNAMICS--------------------
#Part1: individual analysis
## Tidy data for colony movement
#[colony_size_main1]----
colony_size_main1 <- colony_member_main1 %>% 
  group_by(year, colony) %>% 
  summarise(n_birds = n())


#Individual colony movement
#[cm_ind_ad]----
#n=724
cm_ind_ad <- colony_member_main1 %>% 
  select(-age, -plot) %>% 
  group_by(year, color) %>% 
  pivot_wider(., id_cols = c("year", "colony", "color", "sex"), 
              names_from = "year", values_from = "colony", values_fill = list(colony = NA)) %>% 
  pivot_longer(., cols = c(`2013`, `2014`, `2015`, `2016`, `2017`), 
               names_to = "period_t0", values_to = "colony_t0") %>%  #$period_t0
  mutate(period_t1 = recode(period_t0, 
                            "2013"="2014", "2014"="2015", "2015"="2016", "2016"="2017", "2017"=""), 
         period = recode(period_t0, 
                         "2013"="y34", "2014"="y45", "2015"="y56", "2016"="y67", "2017"="")) %>%
  #add colony for time_t0
  left_join(select(colony_member_main1, year, color, colony),
            by = c("period_t1" = "year", "color")) %>%  #add colony_t1
  rename(colony_t1 = colony) %>% #which colony do they belong at t1
  filter(!is.na(colony_t0) & period_t0 != "2017") %>% 
  mutate(plot_t0 = substr(colony_t0, 1, 4), 
         plot_t1 = substr(colony_t1, 1, 4)) %>% 
  mutate(move01 = ifelse(colony_t0 == colony_t1, 0, 1)) %>% # $move01: move or not
  mutate(move_site = ifelse(plot_t0 == plot_t1, 0, 1)) %>% 
  mutate(missing01 = ifelse(is.na(colony_t1), 1, 0)) %>% #missing next year
  setDT()

#summarise # of birds at each colony at t1
#[n_bird_colony_move_t1]----
n_bird_colony_move_t1 <- cm_ind_ad %>% 
  filter(!is.na(colony_t1)) %>% 
  count(period_t0, colony_t0, colony_t1) %>% 
  rename(n_birds = n)

#summarise # of colonies birds moved to at t1
#[n_colony_move_t1]----
n_colony_move_t1 <- n_bird_colony_move_t1 %>% 
  count(period_t0, colony_t0) %>% 
  rename(n_colony_t1 = n)


#------Tidy COLONY STATUS-----
#how many inds move
colony_move_n <- cm_ind_ad %>% 
  count(period_t0, colony_t0, move01) %>% 
  group_by(period_t0, colony_t0) %>% 
  mutate(size_t0 = sum(n)) %>%
  mutate(period_t1 = as.character(as.numeric(period_t0) + 1)) %>% 
  left_join(colony_size_main1, by = c("period_t1" = "year", "colony_t0" = "colony")) %>% 
  rename(size_t1 = n_birds) %>% 
  mutate(size_t1 = replace_na(size_t1, 0)) %>% 
  setDT()

#[colony_status]----
colony_status <- colony_move_n %>% 
  pivot_wider(., names_from = move01, values_from = n) %>% 
  mutate(period = recode(period_t0, 
                  "2013"="y34", "2014"="y45", "2015"="y56", "2016"="y67", "2017"="")) %>% 
  rename("n_move" = `1`, "n_stay"=`0`, "n_gone" = `NA`) %>% 
  mutate_at(vars(n_move, n_stay, n_gone), replace_na, 0) %>% 
  left_join(n_colony_move_t1, by = c("period_t0", "colony_t0")) %>% 
  mutate(n_colony_t1 = replace_na(n_colony_t1, 0)) %>% 
  mutate(per_50gone = round(n_gone/size_t0, digits = 3)) %>% 
  mutate(colony_status_RD = ifelse(n_stay == 0 & size_t1 == 0, "dissolved", "remained")) %>%  #remained or dissolved
  mutate(FF_colony = NA) %>% 
  mutate( #dissolved = nobody stayed
    FF_colony = replace(FF_colony, colony_status_RD == "dissolved" & n_move == 0, "all gone"),
    FF_colony = replace(FF_colony, colony_status_RD == "dissolved" & size_t1 == 0 & n_move > 0 & n_colony_t1 == 1 & per_50gone < 0.5, "move together"),
    FF_colony = replace(FF_colony, colony_status_RD == "dissolved" & size_t1 == 0 & n_move > 0 & n_colony_t1 == 1 & per_50gone >= 0.5, "fission"),
    FF_colony = replace(FF_colony, colony_status_RD == "dissolved" & size_t1 == 0 & n_move > 0 & n_colony_t1 > 1, "fission"),
    FF_colony = replace(FF_colony, colony_status_RD == "remained" & n_stay == 0 & size_t1 > 0, "new"), 
    FF_colony = replace(FF_colony, colony_status_RD == "remained" & n_stay > 0, "fusion"), 
    FF_colony = replace(FF_colony, colony_status_RD == "remained" & n_stay > 0 & (n_move > 0 | per_50gone > 0), "fission-fusion")
  ) %>% 
  setDT()

#write_xlsx(colony_status, "../output/analysis_dynamics2/data/output_colony_status.xlsx")

#summarise # of birds at each colony at t1
#[n_bird_colony_move_t1]----
n_bird_colony_move_t1 <- cm_ind_ad %>% 
  filter(!is.na(colony_t1)) %>% 
  count(period_t0, colony_t0, colony_t1) %>% 
  rename(n_birds = n)

#summarise # of colonies birds moved to at t1
#[n_colony_move_t1]----
n_colony_move_t1 <- n_bird_colony_move_t1 %>% 
  count(period_t0, colony_t0) %>% 
  rename(n_colony_t1 = n)


# % of birds missing by COLONY
#[per_mis_birds_colony]----
per_mis_birds_colony <- cm_ind_ad %>% 
  mutate(ycolony = paste0(period_t0, colony_t0)) %>% 
  count(period_t0, colony_t0, ycolony, missing01) %>% 
  group_by(ycolony) %>% 
  mutate(total_birds = sum(n), per = round(n/total_birds, digits = 3)) %>% 
  filter(missing01==1)

#number of birds in each year (Table1)
#[n_birds_year][per_birds_missing]----
n_birds_year <- cm_ind_ad %>% count(period_t0) %>% print()
per_birds_missing <- cm_ind_ad %>% 
  count(period_t0, missing01) %>% 
  group_by(period_t0) %>% 
  mutate(sum = sum(n), per_missing = round(n/sum, 3)) %>% 
  arrange(period_t0, missing01)

#write_xlsx(per_birds_missing, "../output/analysis_dynamics2/data/output_per_mising birds.xlsx")

per_birds_missing_yearplot <- cm_ind_ad %>% 
  count(plot_t0, period_t0, missing01) %>% 
  group_by(plot_t0, period_t0) %>% 
  mutate(sum = sum(n), per_missing = round(n/sum, 3)) %>% 
  arrange(period_t0, missing01)

#write_xlsx(per_birds_missing_yearplot, "../output/analysis_dynamics2/data/output_per_mising birds_yearplot.xlsx")

#add SD
sum_year_per_birds_missing <- per_birds_missing_yearplot %>% 
  group_by(period_t0, missing01) %>% 
  summarise(mean = mean(per_missing), sd = sd(per_missing))

#write_xlsx(sum_year_per_birds_missing, "../output/analysis_dynamics2/data/output_per_mising birds_SD_across plot.xlsx")

#NOW, let's summarize the colony dynamics-----
colony_status %>% 
  count(colony_status_RD)

#Remained or dissolved (ALL plot and year)
t0 <- colony_status %>% 
  count(colony_status_RD)

t0_period <- colony_status %>% count(period_t0, colony_status_RD) %>% group_by(period_t0) %>% mutate(sum = sum(n), pro = round(n/sum, digits = 3), pro100 = pro*100) %>% setDT()

sum_t0_period <- t0_period %>% group_by(colony_status_RD) %>% summarise(mean_pro = mean(pro), sd_pro = sd(pro))

t0_period_t <- as.data.frame(xtabs(~ period + colony_status_RD, colony_status)) %>% 
  pivot_wider(., names_from = "period", values_from = "Freq") %>% 
  column_to_rownames(., var = "colony_status_RD") %>% 
  data.matrix()

t0_period_per_t <- 
  as.data.frame(xtabs(~ period + colony_status_RD, colony_status)) %>% 
  group_by(period) %>% 
  mutate(sum = sum(Freq), per = round(Freq/sum, digits = 2)*100) %>% 
  pivot_wider(., id_cols=c(period, colony_status_RD, per), names_from = "period", values_from = "per") %>% 
  column_to_rownames(., var = "colony_status_RD") %>% 
  data.matrix()

##FF or move together
t <- as.data.frame(xtabs(~ colony_status_RD + FF_colony, colony_status)) %>% 
  filter(Freq > 0) %>% 
  group_by(colony_status_RD) %>% 
  mutate(sum = sum(Freq), pro = round(Freq/sum, digits = 3))

##by year (Table: colony dynamics)
t_period <- as.data.frame(xtabs(~ period_t0 + colony_status_RD + FF_colony, colony_status)) %>% 
  filter(Freq > 0) %>% 
  group_by(period_t0) %>% 
  mutate(sum = sum(Freq), pro = round(Freq/sum, digits = 3))

##by year/FFD
#prepare for chi-square test
t_period_FF <- as.data.frame(xtabs(~ period + FF_colony, colony_status)) %>% 
  pivot_wider(., names_from = "period", values_from = "Freq") %>% 
  column_to_rownames(., var = "FF_colony") %>% 
  data.matrix()

t_period_FF_per <- as.data.frame(xtabs(~ period + FF_colony, colony_status)) %>% 
  group_by(period) %>% 
  mutate(sum = sum(Freq), per = round(Freq/sum, digits = 2)*100) %>% 
  pivot_wider(., id_cols=c(period, FF_colony, per), names_from = "period", values_from = "per") %>% 
  column_to_rownames(., var = "FF_colony") %>% 
  data.matrix()
  
  

#------COLONY DYNAMICS(only SPRA)-------
#[colony_status_SPRA]----
colony_status_SPRA <- colony_status %>% 
  mutate(plot = substr(colony_t0, 1, 4)) %>% 
  filter(plot == "SPRA")

#Remained or dissolved (only SPRA)
t0_s <- colony_status_SPRA %>%
  count(colony_status_RD)

t0_period_s <- colony_status_SPRA %>% count(period_t0, colony_status_RD) %>% group_by(period_t0) %>% mutate(sum = sum(n), pro = round(n/sum, digits = 3), pro100 = pro*100) %>% setDT()

sum_t0_period_s <- t0_period_s %>% group_by(colony_status_RD) %>% summarise(mean_pro = mean(pro), sd_pro = sd(pro))

##FF or move together
t_s <- as.data.frame(xtabs(~ colony_status_RD + FF_colony, colony_status_SPRA)) %>% 
  filter(Freq > 0) %>% 
  group_by(colony_status_RD) %>% 
  mutate(sum = sum(Freq), pro = round(Freq/sum, digits = 3))

##by year (Table: colony dynamics)
t_period_s <- as.data.frame(xtabs(~ period_t0 + colony_status_RD + FF_colony, colony_status_SPRA)) %>% 
  filter(Freq > 0) %>% 
  group_by(period_t0) %>% 
  mutate(sum = sum(Freq), pro = round(Freq/sum, digits = 3))

#------COLONY DYNAMICS(only colony with <50% missingness)-------
#remove if don't end up using this one
#[colony_status_50]----
colony_status_50 <- colony_status %>% filter(per_50gone < 0.5)
  
#Remained or dissolved
t0_50 <- colony_status_50 %>%
  count(colony_status_RD)

t0_period_50 <- colony_status_50 %>% count(period_t0, colony_status_RD) %>% group_by(period_t0) %>% mutate(sum = sum(n), pro = round(n/sum, digits = 3), pro100 = pro*100) %>% setDT()

sum_t0_period_50 <- t0_period_50 %>% group_by(colony_status_RD) %>% summarise(mean_pro = mean(pro), sd_pro = sd(pro))

##FF or move together
t_50 <- as.data.frame(xtabs(~ colony_status_RD + FF_colony, colony_status_50)) %>% 
  filter(Freq > 0) %>% 
  group_by(colony_status_RD) %>% 
  mutate(sum = sum(Freq), pro = round(Freq/sum, digits = 3))

##by year (Table: colony dynamics)
t_period_50 <- as.data.frame(xtabs(~ period_t0 + colony_status_RD + FF_colony, colony_status_50)) %>% 
  filter(Freq > 0) %>% 
  group_by(period_t0) %>% 
  mutate(sum = sum(Freq), pro = round(Freq/sum, digits = 3))

#colony dispersion index----
#Number of colonies/number of birds
#not used in the analysis

n_colony <- colony_member %>% 
  distinct(year, plot, colony) %>% 
  count(year, plot) %>% 
  rename(n_colony = n)

n_birds <- colony_member %>% 
  distinct(year, plot, color) %>% 
  count(year, plot) %>% 
  rename(n_birds = n)

disp <- n_colony %>% 
  left_join(n_birds, by=c("year", "plot")) %>% 
  mutate(dispIndex = n_colony/n_birds, 
         attraIndex = 1-dispIndex) %>% 
  left_join(rainfall_period_z, by = c("year" = "period_t1")) %>% 
  setDT()

#########################################
#Dyadic movment
#########################################
#tidy data (FAT dataset)
#cm_dyad_l <- read_csv("../output/analysis_dynamics/data/output_cm_dyad_l_all dyads.csv")

#[cm_dyad_l]----
cm_dyad_l <- aso_ssa %>%  #n= 32573
  filter(gtype == "daytime") %>% 
  select(-gtype) %>% 
  rename(period_t0 = year, aso_D_t0 = association) %>%
  filter(period_t0 != "2017") %>%
  mutate(period_t1 = dplyr::recode(period_t0, 
                            "2013"="2014", "2014"="2015", "2015"="2016", "2016"="2017", "2017"=""), 
         period = dplyr::recode(period_t0, 
                         "2013"="y34", "2014"="y45", "2015"="y56", "2016"="y67", "2017"="")) %>%
  #add age
  left_join(select(age, year, color, age), by = c("period_t1"="year", "id1" = "color")) %>% 
  rename(id1_age_t1 = age) %>% 
  left_join(select(age, year, color, age), by = c("period_t1"="year", "id2" = "color")) %>% 
  rename(id2_age_t1 = age) %>% 
  
  #add colony association_t0
  left_join(select(dyad_aso_N_colony, id12, year, aso_colony), by = c("period_t0" = "year", "id12")) %>% 
  rename(aso_colony_t0 = aso_colony) %>% 
  left_join(select(dyad_aso_N_colony, id21, year, aso_colony), by = c("period_t0" = "year", "id12" = "id21")) %>% 
  mutate(aso_colony_t0 = ifelse(is.na(aso_colony_t0), aso_colony, aso_colony_t0)) %>% 
  select(-aso_colony) %>% 
  #add colony association_t1
  left_join(select(dyad_aso_N_colony, id12, year, aso_colony), by = c("period_t1" = "year", "id12")) %>% 
  rename(aso_colony_t1 = aso_colony) %>% 
  left_join(select(dyad_aso_N_colony, id21, year, aso_colony), by = c("period_t1" = "year", "id12" = "id21")) %>% 
  mutate(aso_colony_t1 = ifelse(is.na(aso_colony_t1), aso_colony, aso_colony_t1)) %>% 
  select(-aso_colony) %>% 
  
  #add association
  left_join(select(aso_ssa[gtype == "sleeping"], year, id12, association), #sleeping association
            by = c("period_t0" = "year", "id12")) %>% 
  rename(aso_N_t0 = association) %>%
  # left_join(select(aso_ssa[gtype == "daytime"], year, id12, association), #daytime association
  #           by = c("period_t0" = "year", "id12")) %>% 
  #rename(aso_D_t0 = association) %>% 
  left_join(select(aso_ssa[gtype == "breeding"], year, id12, association), #breeding association
            by = c("period_t0" = "year", "id12")) %>% 
  rename(aso_B_t0 = association) %>% 
  mutate_at(c("aso_N_t0", "aso_D_t0", "aso_B_t0"), replace_na, 0) %>% 
  
  #add colony for time_t0
  left_join(select(colony_member_AF, year, colony, color), 
            by = c("period_t0" = "year", "id1"="color")) %>%  
  rename("id1colony_t0" = "colony") %>% 
  left_join(select(colony_member_AF, year, colony, color), 
            by = c("period_t0"="year", "id2"="color")) %>% 
  rename("id2colony_t0" = "colony") %>% 
  
  #add colony for time_t1
  left_join(select(colony_member_AF, year, colony, color), 
            by = c("period_t1" = "year", "id1"="color")) %>%  #add colony membership
  rename("id1colony_t1" = "colony") %>% 
  left_join(select(colony_member_AF, year, colony, color), 
            by = c("period_t1"="year", "id2"="color")) %>% 
  rename("id2colony_t1" = "colony") %>% 
  
  ######
#add groupid for time_t0
  left_join(select(mygroup_ssa[gtype=="daytime"], year, plot, id, groupid), 
          by = c("period_t0" = "year", "id1"="id", "plot")) %>%
  rename("id1Dgroup_t0" = "groupid") %>% 
  left_join(select(mygroup_ssa[gtype=="daytime"], year, plot, id, groupid), 
            by = c("period_t0"="year", "id2"="id", "plot")) %>% 
  rename("id2Dgroup_t0" = "groupid") %>% 
  
  #add groupid for time_t1
  left_join(select(mygroup_ssa[gtype=="daytime"], year, plot, id, groupid), 
            by = c("period_t1" = "year", "id1"="id", "plot")) %>%
  rename("id1Dgroup_t1" = "groupid") %>% 
  left_join(select(mygroup_ssa[gtype=="daytime"], year, plot, id, groupid), 
            by = c("period_t1"="year", "id2"="id", "plot")) %>% 
  rename("id2Dgroup_t1" = "groupid") %>% 
  
  #if id1 and id2 are in the same group
  mutate(samegroup_t0 = ifelse(id1Dgroup_t0 == id2Dgroup_t0, 1, 0),
         samegroup_t1 = ifelse(id1Dgroup_t1 == id2Dgroup_t1, 1, 0)) %>% 
  
  #if id1 and id2 are in the same colony
  mutate(samecolony_t0 = ifelse(id1colony_t0 == id2colony_t0, 1, 0),
         samecolony_t1 = ifelse(id1colony_t1 == id2colony_t1, 1, 0)) %>% 
  #add fission fusion variable, #samecolony_dt:1->fusion, -1:fission, 0: no change
  mutate(samecolony_dt = samecolony_t1 - samecolony_t0) %>%  
  mutate(FF_dyad = ifelse(samecolony_dt == -1, "fission", 
                          ifelse(samecolony_dt == 1, "fussion", 
                                 "unchanged"))) %>% 
  
  filter(!is.na(samecolony_t0)) %>% #exclude birds with no colony membership
  mutate(id1_move01 = ifelse(id1colony_t0 != id1colony_t1, 1, 0)) %>% 
  mutate(id2_move01 = ifelse(id2colony_t0 != id2colony_t1, 1, 0)) %>% 
  setDT() %>% 
  #add distance
  mutate(dist_t0 = fun_weight(.[, .(id1colony_t0, id2colony_t0)], colony_dist, "dist_m"), 
         dist_t1 = fun_weight(.[, .(id1colony_t1, id2colony_t1)], colony_dist, "dist_m")) %>%
  as.data.frame() %>% 
  mutate(dist_dt = dist_t1 - dist_t0) %>% 
  #add disturbance history
  left_join(select(disturbance, colony, period, disturbance01), 
            by = c("id1colony_t0" = "colony", "period")) %>% 
  rename(id1colony_disturb01 = disturbance01) %>% 
  left_join(select(disturbance, colony, period, disturbance01), 
            by = c("id2colony_t0" = "colony", "period")) %>% 
  rename(id2colony_disturb01 = disturbance01) %>% 
  mutate_at(c("id1colony_disturb01", "id2colony_disturb01"), replace_na, 0) %>% 
  
  #add colony_status
  left_join(select(colony_status, period, colony_t0, colony_status_RD), 
            by = c("id1colony_t0" = "colony_t0", "period")) %>% 
  rename(id1colony_status_RD = colony_status_RD) %>%
  left_join(select(colony_status, period, colony_t0, colony_status_RD), 
            by = c("id2colony_t0" = "colony_t0", "period")) %>% 
  rename(id2colony_status_RD = colony_status_RD) %>%
  #add $FF_colony
  left_join(select(colony_status, period, colony_t0, FF_colony), 
            by = c("id1colony_t0" = "colony_t0", "period")) %>% 
  rename(id1colony_status_FF = FF_colony) %>%
  left_join(select(colony_status, period, colony_t0, FF_colony), 
            by = c("id2colony_t0" = "colony_t0", "period")) %>% 
  rename(id2colony_status_FF = FF_colony) %>%
  
  #add $rainfall
  left_join(rainfall_period_z, by = c("period_t0", "period_t1")) %>% 
  left_join(rainfall_period_LT, by = c("period_t0", "period_t1")) %>% 
  #add $rain_type: wetdry, drydry, drywet, wetwet
  mutate(Lraintype = period) %>% 
  mutate(Lraintype = recode(Lraintype, "y34" = "wetdry", "y45" = "drywet", "y56" = "wetwet", "y67" = "wetdry")) %>% 
  mutate(LSraintype = period) %>% #longrain -> shortrain 
  mutate(LSraintype = recode(LSraintype, 
      "y34" = "wetwet", "y45" = "drydry", "y56" = "wetwet", "y67" = "wetdry")) %>% 
  mutate(R25 = ifelse(r >= 0.25, 1, 0)) %>% 
  left_join(select(disp, period_t0, plot, dispIndex, attraIndex), by = c("period_t0", "plot"))
  

write_xlsx(cm_dyad_l, "../output/analysis_dynamics/data/output_cm_dyad_l_all dyads.xlsx")
write_csv(cm_dyad_l, "../output/analysis_dynamics/data/output_cm_dyad_l_all dyads.csv")

#[cm_dyad_lnna]----
#n=9766, inds have to be present in both years
#this still includes yearlings and $aso_colony will be zero

cm_dyad_lnna <- cm_dyad_l %>% 
  filter(!is.na(samecolony_dt)) %>% #both present in t0 and t1
  #add $yearcolony_t0
  mutate(id1ycolony_t0 = paste0(period_t0, id1colony_t0), 
         id2ycolony_t0 = paste0(period_t0, id2colony_t0),
         id1ycolony_t1 = paste0(period_t1, id1colony_t1),
         id2ycolony_t1 = paste0(period_t1, id2colony_t1)) %>% 
  
  #add % missingness
  left_join(select(per_mis_birds_colony, ycolony, per), by=c("id1ycolony_t0"="ycolony")) %>% 
  rename(id1colony_permis = per) %>% 
  left_join(select(per_mis_birds_colony, ycolony, per), by=c("id2ycolony_t0"="ycolony")) %>% 
  rename(id2colony_permis = per) %>% 
  
  #add colony success
  left_join(colony_suc[season_nstl=="long_rain", .(year, colony, colony_suc)], by=c("period_t0"="year", "id1colony_t0"="colony")) %>% 
  rename(id1colony_suc = colony_suc) %>% 
  left_join(colony_suc[season_nstl=="long_rain", .(year, colony, colony_suc)], by=c("period_t0"="year", "id2colony_t0"="colony")) %>% 
  rename(id2colony_suc = colony_suc) %>% 
  mutate(id1colony_suc=replace_na(id1colony_suc, 0), 
         id2colony_suc=replace_na(id1colony_suc, 0)) %>% 
  
  mutate(aso_colony_t1_2 = replace(aso_colony_t1, aso_colony_t1==1, 0.99)) %>% #for model
  mutate(dist_dt_z = scale(dist_dt),
         dist_t0_z = scale(dist_t0),
         dist_t1_z = scale(dist_t1)) 

write_xlsx(cm_dyad_lnna, "../output/analysis_dynamics/data/output_cm_dyad_lnna_all dyads_present_t0t1.xlsx")

#[d]----
#include all move and stay inds.(adults and yearlings)
#prepare for data analysis
d <- cm_dyad_lnna %>% setDF() %>% 
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d, "../output/analysis_dynamics/data/output_colony_move&stay_all_dyad_new.xlsx")

#[d.a]----
#ALL adults
d.a <- cm_dyad_lnna %>% setDF() %>%
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%  #both are adults at t1
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.a, "../output/analysis_dynamics/data/output_colony_move&stay_alldyad_new.xlsx")


#[d.a.mis]----
d.a.mis <- d.a %>% as.data.frame() %>% 
  filter(!(plot=="LLOD" & period=="y56")) %>% 
  filter(!(plot=="MSTO" & period=="y34")) %>% 
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.a.mis, "../output/analysis_dynamics/data/output_colony_move&stay_alldyad_new_60missing.xlsx")

#[d.a.rall.D]----
#dry for rainfall in transitaional period
d.a.rall.D <- d.a %>% as.data.frame() %>% 
  filter(rainfall_all_z < 0) %>% 
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.a.rall.D, "../output/analysis_dynamics/data/output_colony_move&stay_alldyad_rainfall_all_dry.xlsx")

#[d.a.rall.W]----
#wet for rainfall in transitional period
d.a.rall.W <- d.a %>% as.data.frame() %>% 
  filter(rainfall_all_z > 0) %>% 
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.a.rall.W, "../output/analysis_dynamics/data/output_colony_move&stay_alldyad_rainfall_all_wet.xlsx")

#[d.m1]----
#either of birds moved! adults only
d.m1 <- d.a %>% as.data.frame() %>% 
  filter(id1_move01==1 | id2_move01==1) %>% 
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%  #both are adults
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.m1, "../output/analysis_dynamics/data/output_colony_either_moved.xlsx")

#[d.m1.mis]----
#either of birds moved! adults only, <60%missingness
d.m1.mis <- d.a %>% as.data.frame() %>% 
  filter(!(plot=="LLOD" & period=="y56")) %>% 
  filter(!(plot=="MSTO" & period=="y34")) %>%
  filter(id1_move01==1 | id2_move01==1) %>% 
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%  #both are adults
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.m1.mis, "../output/analysis_dynamics/data/output_colony_either_moved_60missingness.xlsx")
  
#[d.np]----
#include all move and stay inds.(adults), but non-pair
#not sure what you can do about this
d.np <- cm_dyad_lnna %>% setDF() %>%
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%  #both are adults at t1
  filter(pair == 0) %>% 
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.np, "../output/analysis_dynamics/data/output_colony_move&stay_nonpairdyad_new.xlsx")

#[d.m]----
#both moved, boht adults
d.m <- d %>% setDF() %>% 
  filter(id1_move01 == 1 & id2_move01 == 1) %>% #if both move
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%  #both are adults
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.m, "../output/analysis_dynamics/data/output_colony_bothmove_auldt_alldyad_new.xlsx")



#[d.mnp]----
#both move, adults only, non-pairs
d.mnp <- d.m %>% filter(pair == 0) %>%  #exclude pairs
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.mnp, "../output/analysis_dynamics/data/output_colony_move_nonpairdyad_new.xlsx")

#[d.dis.np]----
#dissolved colonies only, non-pair, adults
d.dis.np <- cm_dyad_lnna %>% setDF() %>%
  filter(id1colony_status_RD=="dissolved" & id2colony_status_RD=="dissolved") %>% 
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%  #both are adults
  filter(pair == 0) %>% 
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.dis.np, "../output/analysis_dynamics/data/output_colony_dissolved colony_nonpairdyad_new.xlsx")



#[d.dis.wi]----
#dissolved with pairs, only dyads WITHIN colonies 
d.dis.wi <- cm_dyad_lnna %>% setDF() %>%
  filter(id1colony_status_RD=="dissolved" & id2colony_status_RD=="dissolved") %>% 
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%
  filter(id1colony_t0 == id2colony_t0) %>% 
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.dis.wi, "../output/analysis_dynamics/data/output_colony_dissolved colony_within dyads.xlsx")

#[d.allm.wi]----
#colonies that all birds MOVED = dissolving colonies + new colonies
#with pairs, only dyads WITHIN colonies 
d.allm.wi <- cm_dyad_lnna %>% setDF() %>%
  filter(
    (id1colony_status_RD=="dissolved"&id2colony_status_RD=="dissolved")|
    (id1colony_status_FF=="new"&id2colony_status_FF=="new")) %>% 
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%
  filter(id1colony_t0 == id2colony_t0) %>%   #only within colonies
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.allm.wi, "../output/analysis_dynamics/data/output_colony_dissolved and new colony_within dyads.xlsx")

#[d.allm]----
#colonies that all birds MOVED = dissolving colonies + new colonies
#with pairs, dyads WITHIN and ACROSS colonies 
d.allm <- cm_dyad_lnna %>% setDF() %>%
  filter(
    (id1colony_status_RD=="dissolved"&id2colony_status_RD=="dissolved")|
      (id1colony_status_FF=="new"&id2colony_status_FF=="new")) %>% 
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%  #exclude possible DISPERSAL
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.allm, "../output/analysis_dynamics/data/output_colony_dissolved and new colony_within and across dyads.xlsx")

#[d.allm.mis]----
#moving colonies, both&within, missingness<60%
d.allm.mis <- cm_dyad_lnna %>% setDF() %>%
  filter(
    (id1colony_status_RD=="dissolved"&id2colony_status_RD=="dissolved")|
      (id1colony_status_FF=="new"&id2colony_status_FF=="new")) %>% 
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%
  filter(!(plot=="LLOD" & period=="y56")) %>% 
  filter(!(plot=="MSTO" & period=="y34")) %>% 
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.allm.mis, "../output/analysis_dynamics/data/output_colony_dissolved and new colony_within and across dyads_60missingness.xlsx")

#[d.allm.mis50]----
#only colony <50% missingness
#n=179!
d.allm.mis50 <- cm_dyad_lnna %>% setDF() %>%
  filter(
    (id1colony_status_RD=="dissolved"&id2colony_status_RD=="dissolved")|
      (id1colony_status_FF=="new"&id2colony_status_FF=="new")) %>% 
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%
  filter(id1colony_permis < 0.5) %>% 
  filter(id2colony_permis < 0.5) %>% 
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.allm.mis50, "../output/analysis_dynamics/data/output_colony_dissolved and new colony_within and across dyads_colony50missingness.xlsx")

#[d.allm.rall.D]----
#dry, overall rainfall
d.allm.rall.D <- d.allm %>% as.data.frame() %>% 
  filter(rainfall_all_z < 0) %>%
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.allm.rall.D, "../output/analysis_dynamics/data/output_colony_dissolved and new colony_within and across dyads_rainfall all_dry.xlsx")


#[d.allm.rall.W]----
#wet
d.allm.rall.W <- d.allm %>% as.data.frame() %>% 
  filter(rainfall_all_z > 0) %>%
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0))

write_xlsx(d.allm.rall.W, "../output/analysis_dynamics/data/output_colony_dissolved and new colony_within and across dyads_rainfall all_wet.xlsx")

#either of birds moved! adults only
#[d.m1.np]----
#dissolved colonies only, non-pair, adults
d.m1.np <- cm_dyad_lnna %>% setDF() %>%
  filter(id1_move01==1 | id2_move01==1) %>% 
  filter(id1_age_t1 == "A" & id2_age_t1 == "A") %>%  #both are adults
  filter(pair == 0) %>% 
  mutate(dist_dt_z = scale(dist_dt), 
         r_z = scale(r), 
         aso_D_t0_z = scale(aso_D_t0), 
         aso_N_t0_z = scale(aso_N_t0), 
         aso_colony_t0_z = scale(aso_colony_t0)) 

write_xlsx(d.m1.np, "../output/analysis_dynamics/data/output_colony_either_moved_nonpairdyad.xlsx")

#GO TO analysis_dynamics_MCMCglmm.rmd for further analysis

#exploratory analysiss
test <- d.mnp %>% count(plot, period_t0, samecolony_t1) %>% group_by(plot, period_t0) %>% mutate(sum = sum(n), per = n/sum)

############################
#    GROUP LEVEL ANALYSIS
############################
#refer to the analysis_dynamics_cont.rmd

#[cm_group]----
#n=774
cm_group <- mygroup_ssa %>% 
  filter(gs>1) %>% 
  filter(gtype == "daytime") %>% #only analyze daytime
  mutate(period_t1 = as.character(as.numeric(year) + 1)) %>% 
  rename(period_t0 = year, 
         groupid_t0 = groupid) %>% 
  left_join(mygroup_ssa[,.(id, year, plot, groupid, gtype)], 
            by = c("id", "period_t1"="year", "plot", "gtype")) %>% 
  rename(groupid_t1 = groupid) %>% 
  left_join(colony_member_main1_1night[,.(year, plot, color, colony, ycolony)], 
            by = c("period_t0"="year", "plot", "id"="color")) %>% 
  rename(colony_t0 = colony, ycolony_t0 = ycolony) %>% 
  left_join(colony_member_main1_1night[,.(year, plot, color, colony, ycolony)], 
            by = c("period_t1"="year", "plot", "id"="color")) %>% 
  rename(colony_t1 = colony, ycolony_t1 = ycolony) %>% 
  group_by(groupid_t0) %>% 
  #add colony_t0 status
  left_join(select(colony_status, period_t0, colony_t0, colony_status_RD, FF_colony), by = c("period_t0","colony_t0")) %>% 
  left_join(select(age, color, year, age), 
            by = c("period_t0"="year", "id"="color")) %>% 
  rename(age_t0 = age) %>% 
  mutate(age_t1 = recode(age_t0, "A"="A", "Y"="A", "IM"="Y", "Y/A"="A")) %>% 
  filter(period_t0 != "2017") %>% #since no 2018 
  setDT()

#[sum_cm_group]----
#summarise # of birds by group by colony
sum_cm_group <-  cm_group %>% 
  filter(gs>1) %>% 
  group_by(plot, groupid_t0, gtype, gs, period_t0, period_t1, colony_t0, colony_t1) %>% 
  summarise(n_birds = n()) 

#[sum_cm_group_per]----
#per of birds by colony_t1
#how to deal with groups from diff colonies
sum_cm_group_per <- sum_cm_group %>%
  group_by(plot, groupid_t0, gtype, gs, period_t0, period_t1, colony_t1) %>% 
  summarise(sum = sum(n_birds)) %>% 
  ungroup() %>% 
  mutate(per = round(sum/gs, digits = 3)) %>% 
  filter(!(is.na(colony_t1)&per==1)) %>%  #exclude colonies totally gone
  setDT()
  
#ADD integrity index
#[sum_cm_group_int]----
sum_cm_group_int <- sum_cm_group_per %>% 
  mutate(piLnPi = per*log(per)) %>% 
  group_by(plot, groupid_t0, gtype, gs, period_t0, period_t1) %>% 
  summarise(H = -1*sum(piLnPi)) %>% 
  mutate(evenIndex = round(H/log(gs),digits = 3), 
         integrity = round(1-evenIndex, digits = 3)) %>% 
  setDT()

#[sum_cm_group_per50]----
#assume NAcolony died if per<0.5 and recalcualte per
sum_cm_group_per50 <- sum_cm_group_per %>%
  filter(!(is.na(colony_t1) & per < 0.5))
  

#[sum_cm_group_pa]----
#presence and absence by colony at period_t1
#n=381
sum_cm_group_pa <- sum_cm_group_per %>%
  filter(gs>1) %>% 
  mutate(pa_t1 = ifelse(is.na(colony_t1),"absent", "present")) %>% 
  group_by(plot, groupid_t0, gtype, gs, period_t0, period_t1, pa_t1) %>% 
  mutate(n_colony = 1) %>% 
  #add $n_colony_t1: number of colony at t1 by present/absent (whether birds are present or missing)
  summarise(per = sum(per), n_colony_t1 = sum(n_colony)) %>%  
  setDT()

#group with 50% present at period_t1
groupid_pa50 <- sum_cm_group_pa %>% 
  filter(gs>1) %>% 
  filter(pa_t1=="present" & per>=0.6)

#-----ONLY MOVING colonies-----------
#[cm_group_mov]----
#dissolved and news colonies
cm_group_mov <- cm_group %>% 
  filter(colony_status_RD=="dissolved"|FF_colony=="new") %>% 
  filter(!is.na(colony_t0)) %>% 
  left_join(sum_cm_group_pa[pa_t1=="present", .(groupid_t0, per)], 
            by="groupid_t0") %>% 
  group_by(groupid_t0) %>% 
  mutate(gs = n())

#[sum_cm_group_mov]----
#summarise # of birds by group by colony
sum_cm_group_mov <-  cm_group_mov %>% 
  filter(gs>1) %>% 
  group_by(plot, groupid_t0, gtype, gs, period_t0, period_t1, colony_t0, colony_t1) %>% 
  summarise(n_birds = n())

#[sum_cm_group_mov_per]----
#per of birds by colony_t1
#how to deal with groups from diff colonies
sum_cm_group_mov_per <- sum_cm_group_mov %>%
  group_by(plot, groupid_t0, gtype, gs, period_t0, period_t1, colony_t1) %>% 
  summarise(sum = sum(n_birds)) %>% 
  ungroup() %>% 
  mutate(per = round(sum/gs, digits = 3)) %>% 
  filter(!(is.na(colony_t1)&per==1)) #exclude colonies totally gone

#ADD integrity index
#[sum_cm_group_mov_int]----
sum_cm_group_mov_int <- sum_cm_group_mov_per %>% 
  mutate(piLnPi = per*log(per)) %>% 
  group_by(plot, groupid_t0, gtype, gs, period_t0, period_t1) %>% 
  summarise(H = -1*sum(piLnPi)) %>% 
  mutate(evenIndex = round(H/log(gs),digits = 3), 
         integrity = round(1-evenIndex, digits = 3),
         integrity_asin = asin(sqrt(integrity))) %>% 
  setDT()

#-----TIDY from here
#[group_int]----
group_int <- sum_cm_group_int %>% 
  left_join(sum_cm_group_pa[pa_t1=="present", .(groupid_t0, per)], 
            by="groupid_t0") %>% 
  left_join(mygroup_ssa_attr[,c(1:17, 25:27)], 
            by=c("plot", "gtype", "gs", "groupid_t0"="groupid")) %>% 
  left_join(rainfall_period_z, by = c("period_t0", "period_t1")) %>% 
  mutate(integrity_asin = asin(sqrt(integrity))) %>% 
  setDT()

#[group_mov_int]----
group_mov_int <- sum_cm_group_mov_int %>% 
  left_join(sum_cm_group_pa[pa_t1=="present", .(groupid_t0, per)], 
            by="groupid_t0") %>% 
  left_join(mygroup_ssa_attr[,c(1:4, 6:17, 25:27)], 
            by=c("plot", "gtype", "groupid_t0"="groupid")) %>% 
  left_join(rainfall_period_z, by = c("period_t0", "period_t1")) %>% 
  setDT()


#criteria: >50%--> assume fission, NOT death
#hold on, here you didn't really apply the 50% rule
#carefully examine your criteria on fission-fusion
df <- sum_cm_group_pa %>% 
  filter(!(pa_t1=="absent" & per == 1)) %>% #exclude groups that completely disappeared
  group_by(groupid_t0) %>%
  #assume they died, not moved, if absece<50%
  mutate(n_colony_50_t1 = replace(n_colony_t1, pa_t1=="absent" & per<.5, 0)) %>% 
  #number of colonies moved to based on diff. assumption
  mutate(sum_colony_pa_t1 = sum(n_colony_t1), #assume absent inds move to unknown colony
         sum_colony_pa50_t1 = sum(n_colony_50_t1)) %>% #if criteria is based on n_colony_50_t1
  filter(pa_t1 == "present") %>% 
  #does the group fission? based on diff. assumption
  mutate(fission_p = ifelse(n_colony_t1 > 1, 1, 0), #if only look at present birds
         fission_pa = ifelse(sum_colony_pa_t1 > 1, 1, 0), #absent = 1colony
         fission_pa50 = ifelse(sum_colony_pa50_t1 > 1, 1, 0)) %>% #>50%absent = 1colony
  left_join(Dgroup_unit, by=c("plot", "gtype", "gs", "groupid_t0"="groupid")) %>% 
  left_join(rainfall_period_z, by = c("period_t0", "period_t1")) %>% 
  setDT()

df.np <- df %>% 
  filter(unit!="pair") %>% #analyzed group size larger than 3
  #filter(per>=0.7) %>% #only groups with >=50% birds recorded
  as.data.table()

