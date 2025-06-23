#Chap2 import and tidy data

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
library(Hmisc)

source("fun_r.R")
source("fun_kinship.R")
source("fun_sex.R")
source("funs_plotnw.R")
source("fun_add_triangle.R")
source("fun_weight.R")

select <- dplyr::select  #deal with masked function
recode <- dplyr::recode 

#--------------Environ. factor-------------------
#[rainfall_LT]----
#long term mean
rainfall_LT <- read_xlsx("../data/2012-2017 Mpala rainfall.xlsx", sheet = "LT") %>% group_by(season) %>% summarise(mean_LT = sum(AVG))

#[rainfall_month]----
rainfall_month <- read_xlsx("../data/2012-2017 Mpala rainfall.xlsx", sheet = "by month")

#[rainfall]----
rainfall <- read_xlsx("../data/2012-2017 Mpala rainfall.xlsx") %>%
  mutate_at("year", as.character) %>% 
  pivot_longer(2:5, names_to = "season", values_to = "rainfall") %>% 
  left_join(rainfall_LT, by = c("season")) %>% 
  mutate(diff_LT = rainfall - mean_LT) %>%  #diff between long term mean
  setDT()

#[rainfall_period]----
rainfall_period <- rainfall %>%
  mutate(year = as.numeric(year)) %>% 
  mutate(period_t0 = ifelse(season %in% c("dry","shortrain", "longrain"), year, year - 1)) %>% 
  mutate(period_t1 = period_t0 + 1) %>% 
  mutate_at(c("year", "period_t0", "period_t1"), as.character) %>% 
  pivot_wider(id_cols = c("period_t0", "period_t1","rainfall"), names_from = "season", values_from = "rainfall") %>% 
  mutate(postbreed = shortrain + prebreed) %>% #postbreeding
  mutate(postbreed_DSP = dry + shortrain + prebreed) %>% #dry + shrotrain + prebreed
  mutate(rainfall_all = longrain + postbreed_DSP) %>% 
  mutate(longrain_t1 = lead(longrain)) %>%
  mutate(LSrain = longrain + shortrain) %>% 
  mutate(LSPrain = longrain + shortrain + prebreed) %>% 
  filter(period_t1 %in% c("2013", "2014", "2015", "2016", "2017")) %>% 
  setDT()

Hmisc::label(rainfall_period$postbreed) <- "shortrain + prebreed"
Hmisc::label(rainfall_period$postbreed_DSP) <- "dry + shortrain + prebreed"
Hmisc::label(rainfall_period$rainfall_all) <- "longrain + postbreed_DSP"

#[rainfall_period_LT]----
#long term mean diff
rainfall_period_LT <- rainfall %>%
  mutate(year = as.numeric(year)) %>% 
  mutate(period_t0 = ifelse(season %in% c("shortrain", "longrain"), year, year - 1)) %>% 
  mutate(period_t1 = period_t0 + 1) %>% 
  mutate_at(c("year", "period_t0", "period_t1"), as.character) %>% 
  pivot_wider(id_cols = c("period_t0", "period_t1","diff_LT"), names_from = "season", values_from = "diff_LT") %>% 
  mutate(postbreed = shortrain + prebreed, 
         postbreed_DSP = dry + shortrain + prebreed, 
         rainfall_all = longrain + postbreed_DSP) %>% 
  mutate(longrain_t1 = lead(longrain)) %>%
  mutate(LSrain = longrain + shortrain) %>% 
  mutate(LSPrain = longrain + shortrain + prebreed) %>%
  rename_at(3:10, ~paste0(., "_LTD")) %>%
  mutate(relative_rainfall_all = ifelse(rainfall_all_LTD > 0, 1, 0)) %>% 
  filter(period_t1 %in% c("2013", "2014", "2015", "2016", "2017")) %>% 
  setDT()

#standardize rainfall
#[rainfall_period_z]----
rainfall_period_z <- rainfall_period %>% 
  mutate_at(c("prebreed", "longrain", "shortrain", "postbreed", "postbreed_DSP", "longrain_t1", "rainfall_all", "LSrain", "LSPrain"), ~(scale(.) %>% as.vector)) %>%
  rename_at(3:10, ~paste0(., "_z")) %>% 
  mutate(Lraintype_t0 = ifelse(longrain_z > 0, "wet", "dry"), 
         Lraintype_t1 = ifelse(longrain_t1_z > 0, "wet", "dry"), 
         Lraintype = paste0(Lraintype_t0, Lraintype_t1)) %>%
  mutate(Sraintype_t0 = ifelse(shortrain_z>0, "wet", "dry")) %>% 
  mutate(LSraintype = paste0(Lraintype_t0, Sraintype_t0)) %>% 
  mutate(PBraintype_t0 = ifelse(prebreed_z>0, "wet", "dry")) %>%
  mutate(LPraintype_t0 = paste0(Lraintype_t0, PBraintype_t0)) %>% 
  mutate(Lraintype_t0 = factor(Lraintype_t0, levels = c("dry", "wet")),
         Sraintype_t0 = factor(Lraintype_t0, levels = c("dry", "wet")),
         LSraintype = factor(LSraintype, levels = c("drydry", "drywet","wetdry", "wetwet")),
         PBraintype_t0 = factor(PBraintype_t0, levels = c("dry", "wet")), 
         LPraintype = factor(LPraintype_t0, levels = c("drydry","wetdry", "wetwet"))) %>% 
  setDT()

label(rainfall_period_z$postbreed_z) <- "shortrain + prebreed"
label(rainfall_period_z$postbreed_DSP_z) <- "dry + shortrain + prebreed"
label(rainfall_period_z$rainfall_all_z) <- "longrain + postbreed_DSP"
#-----------END of rainfall----------#


#------------social factor--------
#Family unit
#[Dgroup_unit]----
Dgroup_unit <- read_xlsx("../output/analysis_groups_plot/data/output_D_group_unit.xlsx")


#[aso_ssa]----
#association from SYSTEMATIC SAMPLING

aso_ssa <- readr::read_csv("../output/r and association/data/output_2013-2017 weaver r and association_SS_all_yearplot.csv") %>% mutate(year = as.character(year)) %>% setDT()

#[colony_member]----
colony_member <- read_xlsx("../output/colony member/data/output_colony_member.xlsx")

#[colony_member_main]----
colony_member_main <- colony_member %>% filter(main_colony==1) %>% setDT()

#[colony_member_main1]----
#choose colony in the early season if they moved within the season
colony_member_main1 <- colony_member_main %>% 
  group_by(year, color) %>% 
  dplyr::slice(1) %>% 
  select(year, plot, colony, color, sex, age, season_nstl) %>% 
  ungroup() %>% 
  setDT()

#[colony_member_main_1night]----
colony_member_main1_1night <- read_xlsx("../output/colony member/data/output_colony_member_main1_1night.xlsx") %>% mutate(ycolony = paste0(year, colony)) %>%  setDT()

#---group level data-----
#[group_bycolony_earlylate]----
group_bycolony_earlylate <- read_xlsx("../output/analysis_groups_plot/data/output_unit type within colonies_earlylate.xlsx")

#[group_bycolony]----
group_bycolony <- read_xlsx("../output/analysis_groups_plot/data/output_unit type within colonies.xlsx")

#[nstl]----
nstl <- read_xlsx("../data/output_2012-2017 nestling list.xlsx")

# import age[age]----
age <- read_xlsx("../output/data/output_2012-2017 weaver age.xlsx")

# import sex[sex]----
sex <- read_xlsx("../data/2012-2017 weaver sex_from access.xlsx") %>% setDT()

#import colony distance[colony_dist]----
colony_dist <- read_xlsx("../output/output_2012-2017 weaver colony distance_repeated.xlsx")

#[colony_nstl_LR]----
#colony where long-rain nestling were born (only ones with colors)
colony_nstl_LR <- nstl %>% 
  filter(fledge == 1) %>% 
  select(year_nstl, plot, colony, color) %>%
  rename(year = year_nstl) %>% 
  mutate(sex = fun_sex(color, sex),
         age = "IM", 
         season_nstl = "long_rain") %>% 
  filter(!is.na(color))

#[colony_member_AF]----
#adults + fledglings (year when birds are born)
colony_member_AF <- rbind(colony_member_main1, colony_nstl_LR) %>%
  filter(year != "2012") %>%  #we don't deal with data in 2012 
  arrange(year)

#[nest_list]----
#nest_list <- read_xlsx("../data/2013-2017 weaver nest list.xlsx")
nest_list <- data %>% select(year, plot, colony, nest) %>% distinct()

#[dyad_aso_N_colony]----
#nighttime association with colonies, systematic sampling dates only, by PLOT
dyad_aso_N_colony <- read_xlsx("../output/colony_member_temporal/data/output_dyad_aso_N_colony_2020-07-28.xlsx") %>% 
  mutate(id12 = paste0(id1, id2), id21 = paste0(id2, id1)) %>% setDT()

#[mygroup_ssa]----
mygroup_ssa <- read_xlsx("../output/analysis_groups_plot/data/output_2013-2017 weaver group_SS_all_hc.xlsx") %>% select(-group) %>% setDT()

#[disturbance]----
#disturbance hx(elephant)
disturbance <- read_xlsx("../data/c_disturbance_hx.xlsx") %>% setDT()

#[colony_suc]----
#colony success
colony_suc <- colony_member_AF %>% filter(age=="IM") %>% select(year, colony, season_nstl) %>% distinct() %>% mutate(colony_suc=1) %>% setDT()
