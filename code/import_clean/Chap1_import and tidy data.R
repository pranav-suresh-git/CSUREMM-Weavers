#import packages/functions-----
#need to check if all needed
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)
library(skimr)
library(lubridate)
library(chron)
library(data.table)
library(beepr)
library(stringr)
library(tibble)
library(ggpubr)
library(knitr)
library(igraph)
library(hablar)
library(descr)

source("fun_r.R")
source("fun_kinship.R")
source("fun_sex.R")
source("funs_plotnw.R")
source("fun_add_triangle.R")
source("fun_weight.R")

select <- dplyr::select  #deal with masked function
recode <- dplyr::recode 

#-------import logger data-------
data <- readRDS("../data/2013-2017weaver_logger.RData") %>% setDT()  #tester not included

#import sampling duration
duration_n <- read_xlsx("../output/data/output_sampling duration by nestdate.xlsx")


#------Daytime and Nighttime------
# [daytime]----
dstart = "6:30:00"   
dend = "18:00:00"

daytime <-  
  data %>%
  filter(time > dstart & time <= dend) %>%
  mutate(nest_date = paste0(nest, "_", format(date, "%m%d%Y")), 
         date = as.Date(date)) %>% 
  left_join(duration_n, by = c("year", "plot", "colony","nest", "date")) %>% #add sampling duration
  mutate(sec = if_else(!is.na(AM_start) & timestamp.1m >= AM_start & timestamp.1m <= AM_stop_final, 1, 
               if_else(!is.na(PM_stop) & timestamp.1m >= PM_start_final_adj & timestamp.1m <= PM_stop, 2, 0))) %>%   #add sampling section $sec(0: out of sampling range)
  select(year, plot, colony, nest , date, time, kring, color, timestamp.1m, sec, nest_date, year_nstl) %>% 
  mutate(nestdate_sec = paste0(nest_date, "_s", sec)) %>%   #add section info on nestdate $nestdate_sec

# import breeding daily[BD]----
#select code and run
BD <- read_excel("../data/c_output_2012-2017 weaver breeding daily.xlsx") %>% mutate(date = as.Date(date)) %>% setDT()
  
#[dayvisit]----
# Q:here no_visits is NOT standaridized (sampling biased)
dayvisit <- 
  daytime %>% 
  group_by(year, plot, colony, color, nest, date, nest_date) %>% 
  summarise(no_visits = sum(n())) %>% 
  left_join(BD[,.(nest_date, nestid, stage)], by="nest_date") %>% #add $nestid, $stage
  setDT()

#------Nighttime------
#source("00_sleepnest.R") # this file output confirmed sleep nests[sleepnest]
#[sleepnest]----
sleepnest <- read_xlsx("../output/sleepnest/output_2013-2017 weaver sleepnest_2020-06-28.xlsx") %>% 
  mutate(nest_sleepdate = paste0(nest, "_", format(sleep_date, "%m%d%Y"))) %>%  #add $nest_sleepdate
  left_join(BD[, .(nest_date, nestid, stage)], by=c("nest_sleepdate" = "nest_date")) %>% #add $nestid, $stage
  select(year, plot, color, everything()) %>% 
  mutate(sleep_date = as.Date(sleep_date)) %>% 
  setDT() #n = 10484

#[sleepnest_ss]----
sleepnest_ss <- read_xlsx("../output/data/output_2013-2017 weaver sleepnest_ss.xlsx")

#----
#[pair]----
pair <- read_xlsx("../output/finding pair/output_2013-2017 weaver pair_final_repeated.xlsx") %>% mutate(pair=1) %>% setDT()


#[pair_mf]----
#pair ordered by male-female
pair_mf <- read_xlsx("../output/finding pair/2013-2017 weaver pair_final.xlsx") %>% mutate(pair=1) %>%  setDT()

#--dyadID----
dyadID <- readr::read_csv("../output/data/output_dyadID.csv") %>% setDT()

#---------------networks from plot + systematic samplings + ALL colonies----
#DAYTIME X PLOT X SS
#[Dadjm.pssa][Dnet.pssa]----
Dgbi.pssa <- readRDS("../output/data/network/plot/gmm/gbi_D_plot_gmm_SS_all_SRI_v1.list.RData")
Dadjm.pssa <- readRDS("../output/data/network/plot/gmm/adjm_D_plot_gmm_SS_all_SRI_v1.list.Rdata")
Dnet.pssa <- readRDS("../output/data/network/plot/gmm/net_D_plot_gmm_SS_all_SRI_v1.list.Rdata")

#SLEEPING X PLOT X SS
#[Nadjm.pssa][Nnet.pssa]----
Nadjm.pssa <- readRDS("../output/data/network/plot/adjm_N_plot_BNB_SS_all_SRI_v0.list.RData")

Nnet.pssa <- readRDS("../output/data/network/plot/net_N_plot_BNB_SS_all_SRI_v0.list.RData")

#-----association-----
#[aso_ssa]----
#association from SYSTEMATIC SAMPLING
aso_ssa <- readr::read_csv("../output/r and association/data/output_2013-2017 weaver r and association_SS_all_yearplot.csv") %>% mutate(year = as.character(year), pair=as.character(pair)) %>% setDT()

#------attribute data-------------
#[sex]----
sex <- read_xlsx("../data/2012-2017 weaver sex_from access.xlsx") %>% setDT()

#[age]----
age <- read_xlsx("../output/data/output_2012-2017 weaver age.xlsx")

# [nstl]nestling list----
nstl <- read_xlsx("../data/output_2012-2017 nestling list.xlsx") %>% setDT()

#[relate]----
relate <- readr::read_csv("../data/Relateness_Related_20200116_pop_adult_Wang_Q&G_2ML.csv") %>%
  rename(id1 = ind1.id, id2 = ind2.id) %>%
  mutate(id1 = str_replace(id1, "AA_", ""), id2 = str_replace(id2, "AA_", "")) %>% 
  mutate(id12 = paste0(id1, id2), id21 = paste0(id2, id1)) %>% 
  select(-pair.no) %>% 
  select(id1, id2, id12, id21, everything()) %>% 
  mutate(r = wang) %>% 
  setDT()

#[kinship]----
kinship <- readr::read_csv("../../../../relatedness/kinship/Cervus/R/output/output_weaver_kinship_rWang_kg_target_sim10000_20200420.csv") %>% 
  select(-no) %>% 
  mutate(id12 = paste0(Ind1, Ind2), id21 = paste0(Ind2, Ind1)) %>% 
  as.data.table() %>% 
  mutate(id12sex = fun_sexdyad(.[,.(Ind1, Ind2)], sex)[[3]]) %>% #add $id12sex
  mutate(kin_kg = recode(kin_kg, "DR"="U")) %>% #"DR"(FC, AVC, HS, GPO)
  setDT()  #sim=10000, allele freq: adult

#[nestbird_attr]----
nestbird_attr <- read_xlsx("../output/weaver nestbird attributes.xlsx") %>% setDT()

#[colony_member], n = 1235, only adults
#this only includes birds that sleep >1 night in the colony
#you may want to update this one and use the results in colony_member_temporal.rmd
colony_member <- read_xlsx("../output/colony member/data/output_colony_member.xlsx") %>% 
  setDT()

#[colony_member_main]----
colony_member_main <- colony_member %>% filter(main_colony==1) %>% setDT()

#[colony_member_main1]----
colony_member_main1 <- colony_member_main %>% 
  group_by(year, color) %>% 
  dplyr::slice(1) %>% 
  select(year, plot, colony, color, sex, age, season_nstl) %>% 
  ungroup() %>% 
  setDT()

#[colony_member_month]----
#this inlcudes birds sleeping =1night
colony_member_month <- read_xlsx("../output/colony member/data/output_colony_member_month.xlsx") %>% setDT()


#[colony_size]----
colony_size <- 
  read_xlsx("../output/colony member/data/output_colony_size_2019-11-26.xlsx") %>%
  filter(n_birds > 0) %>% 
  mutate(size = if_else(n_birds <=10, "S", 
                        if_else(n_birds >10 & n_birds <=25, "M", 
                                if_else(n_birds >25 & n_birds <= 50, "L", 
                                        "XL")))) %>% 
  setDT()

#colony distance----
#[colony_dist]----
colony_dist <- read_xlsx("../data/output_2012-2017 weaver colony distance_repeated.xlsx") %>% 
  mutate(dist_km = dist_m/1000) %>% 
  mutate(plot1 = substr(colony1, 1, 4), plot2 = substr(colony2, 1, 4)) 

#nest distance----
#[nest_dist]----
nestdist <- read_xlsx("../output/spatial_arrange/data/output_nestdist.xlsx")

#[pair]----
pair <- read_xlsx("../output/finding pair/output_2013-2017 weaver pair_final_repeated.xlsx") %>% mutate(pair=1) %>% setDT()

#-------group, systematic sampling-------
#------all colonies-------
#[mygroup_ssa]----
#n=893, n=809 if gs>1
mygroup_ssa <- read_xlsx("../output/analysis_groups_plot/data/output_2013-2017 weaver group_SS_all_hc.xlsx") %>% select(-group) %>% setDT()

#group size
#[mygroup_ssa_gs]----
mygroup_ssa_gs <- read_xlsx("../output/analysis_groups_plot/data/output_2013-2017 weaver groupsize_SS_all_hc.xlsx") %>% setDT()


#[mygroup_ssa_r]----
mygroup_ssa_r <- read_xlsx("../output/analysis_groups_plot/data/output_2013-2017 weaver group_SS_all_r and association.xlsx") %>% mutate(pair = replace_na(pair, 0)) %>% setDT()


#[mygroup_ssa_nw]----
#network features for groups, n = 809
mygroup_ssa_nw <- read_xlsx("../output/analysis_group_plot_nw_features/data/output_mygroup_SS_all_nwfeatures.xlsx")

#[mygroup_ssa_attr]----
#attributes for all groups, n=806
mygroup_ssa_attr <- read_xlsx("../output/analysis_groups_plot/data/output_mygroup_SS_all_attr.xlsx") %>%
  filter(gs>1) %>%
  left_join(mygroup_ssa_nw, by = c("year", "plot", "gtype","groupid")) %>%
  setDT()

#[group_colony]----
group_bycolony_earlylate <- read_xlsx("../output/analysis_groups_plot/data/output_unit type within colonies_earlylate.xlsx")
