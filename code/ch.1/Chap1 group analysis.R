#------GROUP ANALYSIS-------

#import groups data from "cluster analysis"
#systematic sampling
Dg_ssa <- read_xlsx("../output/cluster analysis/data/plot/2013-2017 weaver groups_D_plot_SS_all_gmm_SRI_v1_hc.xlsx") %>% setDT()
Ng_ssa <- read_xlsx("../output/cluster analysis/data/plot/2013-2017 weaver groups_N_plot_SS_all_BNB_SRI_v0_hc.xlsx") %>% setDT()
Bg <- read_xlsx("../output/cluster analysis/data/plot/2013-2017 weaver groups_D_plot_gmm_B_SRI_v1_hc.xlsx") %>% setDT()
Ng_ssa_nb <- read_xlsx("../output/cluster analysis/data/plot/2013-2017 weaver groups_N_plot_SS_all_NB_SRI_v0_hc.xlsx") %>% setDT()
Ng_b <- read_xlsx("../output/cluster analysis/data/plot/2013-2017 weaver groups_N_plot_B_SRI_v0_hc.xlsx") %>%  setDT()
Bg_nstl <- read_xlsx("../output/cluster analysis/data/plot/2013-2017 weaver groups_D_plot_NSTL_v3_hc.xlsx") %>% setDT()

groups <- rbind(Dg_ssa, Ng_ssa, Bg) %>% select(-group) %>%  rename(id = color)

#[mygroup]----
mygroup <- groups %>% 
  filter(groupid != "17MDg2" & groupid != "17MNg2") %>% #remove groups with MYPO (no genetic data)
  left_join(select(nestbird_attr, node, sex), by = c("id" = "node")) %>% # add $sex
  add_count(gtype, groupid) %>% rename("gs" = "n") %>%   # add $group size
  mutate(year_nstl = nstl[match(id, nstl$color)]$year_nstl) %>%  #add $year_nstl
  mutate(pre_nstl = as.numeric(year) - as.numeric(year_nstl)) %>%   #how many years ago was the bird born?
  setDT() #n=1485(cutoff2)

#write_xlsx(mygroup, paste0("../output/analysis_groups_plot/data/output_2013-2017 weaver group_SS_all_hc.xlsx"))


#-------GROUP SIZE---------------
#[mygroup_gs]----
mygroup_gs <- mygroup %>% 
  group_by(year, plot, gtype, groupid) %>% 
  summarise(gs=n()) %>% 
  ungroup() %>% setDT()

#write_xlsx(mygroup_gs, paste0("../output/analysis_groups_plot/data/output_2013-2017 weaver groupsize_SS_all_hc",".xlsx"))

sum_mygroup_gs_yearplot <- mygroup_gs %>% filter(gs>1) %>% group_by(year, plot, gtype) %>% summarise(mean = mean(gs), sd = sd(gs), median=median(gs), max=max(gs), n = n()) 

sum_mygroup_gs_plot <- mygroup_gs %>% filter(gs>1) %>% group_by(gtype, plot) %>% summarise(mean = mean(gs), sd = sd(gs), firstQ = quantile(gs, 0.25), thirdQ = quantile(gs, 0.75), n = n()) %>% setDT()

#write_xlsx(sum_mygroup_gs_yearplot, "../output/analysis_groups_plot/data/output_sum_mygroup_SS_all_gs_yearplot_hc.xlsx"))

#------GROUP RELATEDNESS--------
# generate combo list for each group and add attributes
f <- function(data){
  as.data.frame.matrix(t(combn(data$id, 2)), stringsAsFactors=F) %>% 
    mutate(V1sex = fun_sexdyad(., sex)[[1]]) %>%  #add $sex
    mutate(V2sex = fun_sexdyad(., sex)[[2]]) %>%  
    mutate(V12sex = fun_sexdyad(., sex)[[3]]) %>% 
    mutate(r = fun_r(., relate)) %>%  #add $r
    mutate(kinorder = fun_kinship(., kinship, "kinorder")) %>% #add $kinship
    mutate(kinorder2 = fun_kinship(., kinship, "kinorder2")) 
}

#[mygroup_nested]----
#this takes super long
mygroup_nested <- mygroup %>% 
  select(year, plot, gtype, groupid, gs, id) %>% 
  group_by(year, plot, gtype, groupid, gs) %>% 
  filter(gs > 1) %>% 
  nest() %>% 
  mutate(comb.list = map(data, f)) 

#[mygroup_r]----
#add r
mygroup_r <- mygroup_nested %>% select(-data) %>% unnest(cols = c(comb.list)) %>%
  left_join(select(pair, V1, V2, year, plot, pair), by=c("year", "plot", "V1", "V2")) %>%
  left_join(select(aso_ssa, id1, id2, year, plot, gtype, association), 
            by = c("year", "plot", "gtype", "V1"="id2", "V2"="id1")) %>%  #add association
  left_join(select(age, year, color, age), by = c("year", "V1"="color")) %>%  #add $age
  left_join(select(age, year, color, age), by = c("year", "V2"="color"), suffix= c("V1", "V2")) 

#write_xlsx(mygroup_r, "../output/analysis_groups_plot/data/output_2013-2017 weaver group_SS_all_r and association.xlsx"))

#group attributes----
#r
groupid_r <- mygroup_r %>% group_by(year, plot, gtype, groupid) %>% summarise(mean_r = mean(r))

groupid_r_sex <- mygroup_r %>% mutate(V12sex = as.character(V12sex)) %>%  group_by(year, plot, gtype, groupid, V12sex) %>% summarise(mean_r = mean(r))

groupid_r_sex_w <- groupid_r_sex %>% 
  pivot_wider(., names_from = "V12sex", values_from = "mean_r") %>% 
  #select(-"NA") %>% 
  rename(r_ff = ff, r_mf = mf, r_mm = mm)

#aso
groupid_aso <- mygroup_r %>% group_by(year, plot, gtype, groupid) %>% summarise(mean_aso = mean(association)) %>% setDT()

groupid_aso_sex <- mygroup_r %>% mutate(V12sex = as.character(V12sex)) %>%  group_by(year, plot, gtype, groupid, V12sex) %>% summarise(mean_aso = mean(association))

groupid_aso_sex_w <- groupid_aso_sex %>% 
  pivot_wider(., names_from = "V12sex", values_from = "mean_aso") %>% 
  #select(-"NA") %>% 
  rename(aso_ff = ff, aso_mf = mf, aso_mm = mm)


#[mygroup_sex]----
mygroup_sex <- mygroup[gs > 1] %>% count(year, plot, gtype, groupid, gs, sex) 

mygroup_sex_w <- mygroup_sex %>% 
  pivot_wider(., names_from = "sex", values_from = "n") %>% 
  #select(-"NA") %>% 
  mutate(sex_ratio = round(m/f, digits = 2)) %>% 
  rename(n_f = f, n_m = m)

groupid_pair <- mygroup_r %>% group_by(groupid) %>% summarise(n_pair=sum(pair, na.rm=T)) #number of pairs in groups

groupid_kinorder2 <- mygroup_r %>% group_by(groupid, kinorder2) %>% summarise(n=n()) %>% pivot_wider(., values_from = n, names_from = "kinorder2") %>% mutate_all(., ~ replace_na(., 0)) %>% mutate(sum_first = sum(first + PO + FS))

mygroup_attr <- mygroup_sex_w %>% #n=811
  left_join(groupid_pair, by = c("groupid")) %>% 
  left_join(groupid_r, by = c("year","plot", "gtype","groupid")) %>% 
  left_join(groupid_r_sex_w, by = c("year","plot", "gtype","groupid")) %>% 
  left_join(groupid_aso, by = c("year","plot", "gtype","groupid")) %>%
  left_join(groupid_aso_sex_w, by = c("year","plot", "gtype","groupid")) %>%
  left_join(groupid_kinorder2, by = c("groupid"))

#write_xlsx(mygroup_attr, "../output/analysis_groups_plot/data/output_mygroup_SS_all_attr.xlsx")

      
#[mygroup_w]----
#move gtype to columns
mygroup_w <- mygroup %>%
  filter(!(gtype == "daytime" & gs == 1)) %>% #exclude transients
  filter(!(gtype == "breeding" & gs == 1)) %>% 
  filter(!(gtype == "nestling" & gs == 1)) %>% 
  select(-gs) %>% 
  pivot_wider(., names_from = "gtype", values_from = "groupid")

#SUM sleeping groups within daytime----
Ngr_Dgr <- mygroup_w %>%
  filter(!is.na(daytime) & !is.na(sleeping)) %>% 
  group_by(year, plot, daytime, sleeping) %>% 
  summarise(n_bird = n()) %>% 
  group_by(daytime) %>% 
  add_count(n_Ngr = n())

sum_n_Ngr_Dgr <- Ngr_Dgr %>% 
  select(year, plot, daytime, n_Ngr) %>% 
  distinct() %>% ungroup()

sum_n_Ngr_Dgr_year <- sum_n_Ngr_Dgr %>% 
  group_by(year) %>% 
  summarise(mean_Ngr = mean(n_Ngr), sd_Ngr = sd(n_Ngr))

sum_n_Ngr_Dgr_all <- sum_n_Ngr_Dgr %>% 
  summarise(mean_Ngr = mean(n_Ngr), sd_Ngr = sd(n_Ngr), median = median(n_Ngr)) 


view(Ngr_Dgr)
view(sum_n_Ngr_Dgr)
view(sum_n_Ngr_Dgr_year)
view(sum_n_Ngr_Dgr_all)
view(sum2_n_Ngr_Dgr)

#----------output results---------
#[Dgroup_unit][Ngroup_unit]
#Daytime association showed the majority of social units were pairs and nuclear families, followed by extended families and other types of units. 
#group size
sum_gs_D <- mygroup_gs %>%
  filter(gtype == "daytime" & gs> 1) %>% 
  count(year, gs) %>% 
  group_by(year) %>% 
  mutate(pro = round(n/sum(n), digits = 2)) %>% 
  ungroup() %>% 
  select(gs, pro) %>% 
  group_by(gs) %>% 
  summarise(mean = mean(pro), sd = sd(pro))

view(sum_gs_D)

#write_xlsx(sum_gs_D, "../output/analysis_groups_plot/data/output_sum_groupsize_daytime.xlsx")

#---------DEFINE SOCIAL UNITS for groupS--------
#so hard but important
#step family < 2%
#daytime----
Dgroup_unit <- mygroup_attr %>% 
  filter(gtype == "daytime") %>% 
  filter(groupid != "17MDg2") %>% #MYPO no genetic data
  mutate(unit = NA) %>% 
  mutate(unit = replace(unit, gs == 2 & n_pair ==1, "pair")) %>% 
  mutate(unit = replace(unit, gs == 2 & n_pair ==0 & (sum_first>0), "NF")) %>% #n=2
  
  mutate(unit = replace(unit, gs == 3 & n_pair ==1 & (PO==2), "NF")) %>% 
  mutate(unit = replace(unit, gs == 3 & n_pair ==1 & (UR==2 & sum_first==1), "EF")) %>% #step family 
  mutate(unit = replace(unit, gs == 3 & n_pair ==1 & (PO==0&first==1), "EF")) %>% 
  mutate(unit = replace(unit, gs == 3 & n_pair ==1 & (second>0), "EF")) %>% 
  mutate(unit = replace(unit, gs == 3 & n_pair ==1 & (FS==1), "EF")) %>% 
  mutate(unit = replace(unit, gs == 3 & n_pair ==2 & (sum_first>0), "pair")) %>%  #wrong pairs? n=1
  mutate(unit = replace(unit, gs == 3 & n_m ==1 & n_f==2 & (sum_first==0 & second==0), "pair")) %>%  #polygny?
  
  mutate(unit = replace(unit, gs == 4 & n_pair ==1 & (PO==4), "NF")) %>% 
  mutate(unit = replace(unit, gs == 4 & n_pair ==1 & (PO==2&(sum_first+second==5)), "NF")) %>%
  mutate(unit = replace(unit, gs == 4 & n_pair ==1 & (PO==2&(sum_first+second<5)), "EF")) %>% #step family
  mutate(unit = replace(unit, gs == 4 & n_pair >=1 & (PO==0 & sum_first>0), "EF")) %>% 
  mutate(unit = replace(unit, gs == 4 & n_pair >=1 & (sum_first==0&second==0), "UR")) %>% 
  
  mutate(unit = replace(unit, gs == 5 & n_pair ==1 & (FS==3), "NF")) %>% 
  mutate(unit = replace(unit, gs == 5 & n_pair ==1 & (FS==0&sum_first>0), "EF")) %>%
  mutate(unit = replace(unit, gs == 5 & n_pair >=2 & (sum_first>0|second>0|FC>0), "EF")) %>% #pair>2, =>EF 
  
  mutate(unit = replace(unit, is.na(unit) & (sum_first>0|second>0|FC>0), "EF")) %>% 
  mutate(unit = replace(unit, is.na(unit) & (sum_first==0&second==0&FC==0), "UR")) %>% 
  mutate(unit = replace(unit, groupid == "17LDg25", "pair")) %>%   #this needs to be fixed in pair; fixed in data for now
  setDT()

#write_xlsx(Dgroup_unit, "../output/analysis_groups_plot/data/output_D_group_unit.xlsx")
  

#SUMMARY OF daytime----
#sum_unit
sum_Dgroup_unit_all <- Dgroup_unit %>% 
  group_by(unit) %>% 
  summarise(n= n()) %>% 
  mutate(pro = round(n/sum(n), digits = 3))

sum_Dgroup_unit_year <- Dgroup_unit %>% 
  group_by(year, unit) %>% 
  summarise(n = n()) %>% arrange(-n, .by_group = T) %>% 
  group_by(year) %>% 
  mutate(pro = round(n/sum(n), digits = 2))

sum_Dgroup_unit_gs <- Dgroup_unit %>% 
  group_by(gs, unit) %>% 
  summarise(n= n()) %>% 
  mutate(pro = round(n/sum(n), digits = 3)) %>% arrange(gs, -pro)

sum_Dgroup_unit <- sum_Dgroup_unit_year %>% 
  group_by(unit) %>% 
  summarise(n_mean = mean(n), n_sd = sd(n))

 write_xlsx(sum_Dgroup_unit_all, "../output/analysis_groups_plot/data/output_sum_D_group_unit_all.xlsx")
 write_xlsx(sum_Dgroup_unit_year, "../output/analysis_groups_plot/data/output_sum_D_group_unit_year.xlsx")
 write_xlsx(sum_Dgroup_unit, "../output/analysis_groups_plot/data/output_sum_D_group_unit.xlsx")

view(Dgroup_unit)
view(sum_Dgroup_unit)
view(sum_Dgroup_unit_gs)
view(sum_Dgroup_unit_year)
view(sum_Dgroup_unit_all)

#sum n_pairs----
sum_Dgroup_pair_np <- Dgroup_unit %>%
  filter(unit!="pair" & gs>=3) %>% #don't analyze pairs
  group_by(n_pair) %>% 
  summarise(n= n()) %>% 
  mutate(pro = round(n/sum(n), digits = 3))

sum_Dgroup_pair_np_bygs <- Dgroup_unit %>%
  filter(unit!="pair" & gs>=3) %>% 
  group_by(gs, n_pair) %>% 
  summarise(n= n()) %>% 
  mutate(pro = round(n/sum(n), digits = 3))

sum_Dgroup_pair_np_all <- Dgroup_unit %>%
  filter(unit!="pair" & gs>=3) %>% #don't analyze pairs
  summarise(mean_npair = mean(n_pair), sd_npair = sd(n_pair))

view(sum_Dgroup_pair_np)  
view(sum_Dgroup_pair_np_all)  
view(sum_Dgroup_pair_np_bygs) 

#plot ASSOCIATION with type of units(pair, NF, EF)----
df <- groupid_aso_sex %>% 
  filter(gtype == "daytime") %>%  
  left_join(Dgroup_unit[, .(groupid, unit)], by = "groupid")
  
ggerrorplot(df, x="unit", y="mean_aso", color = "V12sex", desc_stat = "mean_sd",
            add = "mean", error.plot = "errorbar",
            add.params = list(size=0.7),
            title = "", ylab="association", xlab = "") +
  theme(axis.title.y = element_text(size=18, face="bold"),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14, face="bold")
  )

ggsave("../output/group_analysis/fig/fig_daytime_aso_unittype.png", width = 5, height = 5)

#plot relatedness with type of units(pair, NF, EF)----
ggerrorplot(Dgroup_unit, x="unit", y="mean_r", desc_stat = "mean_sd",
            add = "mean", error.plot = "errorbar",
            add.params = list(size=0.7),
            title = "", ylab="relatedness", xlab = "") +
  theme(axis.title.y = element_text(size=18, face="bold"),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14, face="bold")
  )

ggsave("../output/group_analysis/fig/fig_daytime_aso_unittype.png", width = 5, height = 5)

#nighttime----
Ngroup_unit <- mygroup_attr %>% 
  filter(gtype == "sleeping") %>% 
  filter(groupid != "17MNg2") %>% #MYPO no genetic data
  mutate(unit = NA) %>% 
  mutate(unit = replace(unit, gs == 2 & n_pair ==1, "pair")) %>% 
  mutate(unit = replace(unit, gs == 2 & n_pair ==0 & (sum_first>0), "NF")) %>% #n=2
  
  mutate(unit = replace(unit, gs == 3 & n_pair ==1 & (PO==2), "NF")) %>% 
  mutate(unit = replace(unit, gs == 3 & n_pair ==1 & (UR==2 & sum_first==1), "EF")) %>% #step family 
  mutate(unit = replace(unit, gs == 3 & n_pair ==1 & (PO==0&first==1), "EF")) %>% 
  mutate(unit = replace(unit, gs == 3 & n_pair ==1 & (second>0), "EF")) %>% 
  mutate(unit = replace(unit, gs == 3 & n_pair ==1 & (FS==1), "EF")) %>% 
  mutate(unit = replace(unit, gs == 3 & n_pair ==2 & (sum_first>0), "pair")) %>%  #wrong pairs? n=1
  mutate(unit = replace(unit, gs == 3 & n_m ==1 & n_f==2 & (sum_first==0 & second==0), "pair")) %>%  #polygny?
  
  mutate(unit = replace(unit, gs == 4 & n_pair ==1 & (PO==4), "NF")) %>% 
  mutate(unit = replace(unit, gs == 4 & n_pair ==1 & (PO==2&(sum_first+second==5)), "NF")) %>%
  mutate(unit = replace(unit, gs == 4 & n_pair ==1 & (PO==2&(sum_first+second<5)), "EF")) %>% #step family
  mutate(unit = replace(unit, gs == 4 & n_pair >=1 & (PO==0 & sum_first>0), "EF")) %>% 
  mutate(unit = replace(unit, gs == 4 & n_pair >=1 & (sum_first==0&second==0), "UR")) %>% 
  
  mutate(unit = replace(unit, gs == 5 & n_pair ==1 & (FS==3), "NF")) %>% 
  mutate(unit = replace(unit, gs == 5 & n_pair ==1 & (FS==0&sum_first>0), "EF")) %>%
  mutate(unit = replace(unit, gs == 5 & n_pair >=2 & (sum_first>0|second>0|FC>0), "EF")) %>% #pair>2, =>EF 
  
  mutate(unit = replace(unit, is.na(unit) & (sum_first>0|second>0|FC>0), "EF")) %>% 
  mutate(unit = replace(unit, is.na(unit) & (sum_first==0&second==0&FC==0), "UR")) %>% 
  setDT()



write_xlsx(Ngroup_unit, "../output/analysis_groups_plot/data/output_N_group_unit.xlsx")

## SUMMARY OF nighttime----
sum_Ngroup_unit_all <- Ngroup_unit %>% 
  group_by(unit) %>% 
  summarise(n= n()) %>% 
  mutate(pro = round(n/sum(n), digits=3))

sum_Ngroup_unit_year <- Ngroup_unit %>% 
  group_by(year, unit) %>% 
  summarise(n = n()) %>% arrange(-n, .by_group = T)

sum_Ngroup_unit_gs <- Ngroup_unit %>% 
  group_by(gs, unit) %>% 
  summarise(n= n()) %>% 
  mutate(pro = round(n/sum(n), digits = 3)) %>% arrange(gs, -pro)

sum_Ngroup_unit <- sum_Ngroup_unit_year %>% 
  group_by(unit) %>% 
  summarise(n_mean = mean(n), n_sd = sd(n))

 write_xlsx(sum_Ngroup_unit_all, "../output/analysis_groups_plot/data/output_sum_N_group_unit_all.xlsx")
 write_xlsx(sum_Ngroup_unit_year, "../output/analysis_groups_plot/data/output_sum_N_group_unit_year.xlsx")
 write_xlsx(sum_Ngroup_unit, "../output/analysis_groups_plot/data/output_sum_N_group_unit.xlsx")

view(Ngroup_unit)
view(sum_Ngroup_unit)
view(sum_Ngroup_unit_year)
view(sum_Ngroup_unit_all)

#------analyze COLONY COMPOSITION------
#summarize types of social units within colonies
group_bycolony_earlylate <- mygroup %>% 
  filter(gtype == "daytime" & gs >= 2) %>% 
  left_join(Dgroup_unit[, .(groupid, unit)], by = "groupid") %>%
  left_join(colony_member_month, by = c("year", "id"="color")) 

write_xlsx(group_bycolony_earlylate, "../output/analysis_groups_plot/data/output_unit type within colonies_earlylate.xlsx")

#[group_bycolony]----
group_bycolony <- group_bycolony_earlylate %>% 
  mutate(colony = colony_early) %>% 
  select(id, year, plot, groupid, gtype, gs, unit, colony) %>% 
  count(year, plot, groupid, gtype, gs, unit, colony) %>% 
  group_by(groupid) %>% 
  arrange(-n, .by_group=T) %>% 
  left_join(colony_size, by = c("year", "plot", "colony")) %>% 
  setDT()

write_xlsx(group_bycolony, "../output/analysis_groups_plot/data/output_unit type within colonies.xlsx")

Ngroup_bycolony <- mygroup %>% 
  filter(gtype == "sleeping" & gs >= 2) %>% 
  left_join(Ngroup_unit[, .(groupid, unit)], by = "groupid") %>%
  left_join(colony_member_main1, by = c("year", "plot", "id"="color", "sex")) %>% 
  select(id, year, plot, groupid, gtype, gs, unit, colony) %>% 
  count(year, plot, groupid, gtype, gs, unit, colony) %>% 
  group_by(groupid) %>% 
  arrange(-n, .by_group=T) %>% 
  slice(1)
  
write_xlsx(Ngroup_bycolony, "../output/analysis_groups_plot/data/output_N_unit type within colonies.xlsx")

#------colony composition analysis------



#-------How do pairs form to become larger units?---------
#non-pair, mated inds association within groups-----
mygroup_paired_MF <- mygroup_r %>% 
  filter(V1!="MYPO" & V2!="MYPO") %>% #no genetic data
  left_join(select(mygroup_attr, groupid, n_pair), by=c("groupid")) %>% 
  filter(gs>=4 & pair==0) %>% #we don't examine pairs 
  #filter(association > 0) %>% #only examine dyads with links 
  select(-pair) %>%
  left_join(select(pair, year, V1, pair), by=c("year", "V1")) %>% 
  left_join(select(pair, year, V2, pair), by=c("year", "V2"), suffix=c("1","2")) %>% 
  filter(pair1==1 & pair2==1) %>% #choose only paired m and paired f
  filter(!(V1=="KYRM"&V2=="YKRM")) %>% #outlier, KYRM visited during lay
  setDT()
  
view(mygroup_paired_MF)

write_xlsx(mygroup_paired_MF, "../output/analysis_groups_plot/data/output_mygroup_paired_MF.xlsx")

#
sum_mygroup_paired_MF <- mygroup_paired_MF %>% 
  group_by(gtype, V12sex) %>% 
  summarise(rMean = mean(r), asoMean = mean(association))

view(sum_mygroup_paired_MF)

write_xlsx(sum_mygroup_paired_MF, "../output/analysis_groups_plot/data/output_sum_mygroup_paired_MF.xlsx")

#-----------STATS---------------------
#analyze non-pair mated dyads within groups
#"We found higher association strength between mated males than between mated between females."
library(MCMCglmm)
library(car)
library(coda)
library(beepr)

source("vif_MCMCglmm.R") #vif.MCMCglmm

#---load MCMC model results----
#Load model results here
mcmcfit <- readRDS("../output/analysis_groups_plot/data/mcmcfit_group_aso_sex_multinomial.rdata")

df.D <- mygroup_paired_MF %>% 
  mutate(dyadID = fun_kinship(.[,c("V1", "V2")], dyadID, "dyadID")) %>% 
  filter(gtype == "daytime") %>% 
  mutate(V1 = as.factor(V1), V2 = as.factor(V2), 
         V12sex=factor(V12sex, levels = c("mf", "ff", "mm")), 
         V12sexF = factor(V12sex, levels = c("ff", "mm", "mf"))) %>% 
  mutate(aso.sucx = as.integer(association*100),
         aso.fail = as.integer(100-(association*100))) %>%  #create binomail data
  as.data.frame()
  
saveRDS(df.D, "../output/analysis_groups_plot/data/df_D.rdata")

#----check who those males are connecting
df.D %>% filter(association > 0.01) %>% group_by(V12sex) %>%
  summarise(mean_aso = mean(association), sd_aso = sd(association))

df.D %>% filter(association > 0) %>% group_by(V12sex) %>% summarise(r_mean = mean(r), r_SD=sd(r), aso_mean = mean(association), aso_SD = sd(association))

df.D %>% filter(association > 0.02) %>% count(kinorder) 

df.D %>% filter(association > 0) %>% count(V12sex, kinorder)

#prepare for multi-membership 
df.D$V1 <- as.factor(c(levels(df.D$V2),as.character(df.D$V1)))[-(1:length(levels(df.D$V2)))] 
df.D$V2 <- as.factor(c(levels(df.D$V1),as.character(df.D$V2)))[-(1:length(levels(df.D$V1)))]

#Model 1: ~ V12sex----
priors1<- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V =1, nu = 0.002), G2 = list(V =1, nu = 0.002)))

mcmcfit <- MCMCglmm(cbind(aso.sucx, aso.fail) ~ V12sex, random = ~ mm(V1 + V2) + groupid, family = "multinomial2", data = df.D, verbose = FALSE, prior=priors1, nitt = 200000, burnin = 5000, thin = 100)


saveRDS(mcmcfit, "../output/analysis_groups_plot/data/mcmcfit_group_aso_sex_multinomial.rdata")


#Model 1.2: ~ V12sex (expanded prior)----
#expanded prior; autocorr for groupid decreased, but not much influence on post. for sex compared to model1
priors1.2<- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V =1, nu = 0.002), G2 = list(V=1, nu=1, alpha.mu=0, alpha.V=1000)))

mcmcfit1.2 <- MCMCglmm(cbind(aso.sucx, aso.fail) ~ V12sex, random = ~ mm(V1 + V2) + groupid, family = "multinomial2", data = df.D, verbose = FALSE, prior=priors1.2, nitt = 200000, burnin = 5000, thin = 100)

saveRDS(mcmcfit1.2, "../output/analysis_groups_plot/data/mcmcfit1.2_group_aso_sexMF_gs_multinomial.rdata")

#read model directly
mcmcfit3 <- readRDS("../output/analysis_groups_plot/data/mcmcfit3_group_aso_sexMF_gs_multinomial.rdata")
mcmcfit3.2 <- readRDS("../output/analysis_groups_plot/data/mcmcfit3.2_group_aso_sexF_gs_multinomial.rdata")


#Model 3: ~ V12sex + group size (MF ref)----
priors3<- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V =1, nu = 0.002), G2 = list(V =1, nu = 0.002)))

mcmcfit3 <- MCMCglmm(cbind(aso.sucx, aso.fail) ~ V12sex + gs, random = ~ mm(V1 + V2) + groupid, family = "multinomial2", data = df.D, verbose = FALSE, prior=priors3, nitt = 200000, burnin = 5000, thin = 100)

mcmcfit3b <- MCMCglmm(cbind(aso.sucx, aso.fail) ~ V12sex + gs, random = ~ mm(V1 + V2) + groupid, family = "multinomial2", data = df.D, verbose = FALSE, prior=priors3, nitt = 200000, burnin = 5000, thin = 100)

saveRDS(mcmcfit3, "../output/analysis_groups_plot/data/mcmcfit3_group_aso_sexMF_gs_multinomial.rdata")
saveRDS(mcmcfit3b, "../output/analysis_groups_plot/data/mcmcfit3b_group_aso_sexMF_gs_multinomial.rdata")

#Model 3.2: ~ V12sex + group size (FF ref)----
priors3<- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V =1, nu = 0.002), G2 = list(V =1, nu = 0.002)))

mcmcfit3.2 <- MCMCglmm(cbind(aso.sucx, aso.fail) ~ V12sexF + gs, random = ~ mm(V1 + V2) + groupid, family = "multinomial2", data = df.D, verbose = FALSE, prior=priors3, nitt = 200000, burnin = 5000, thin = 100)

saveRDS(mcmcfit3.2, "../output/analysis_groups_plot/data/mcmcfit3.2_group_aso_sexF_gs_multinomial.rdata")

#Model 4: ~ V12sex + group size (exp. prior, MF ref)----
priors4<- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V =1, nu = 0.002), G2 = list(V=1, nu=1, alpha.mu=0, alpha.V=1000)))

mcmcfit4 <- MCMCglmm(cbind(aso.sucx, aso.fail) ~ V12sex + gs, random = ~ mm(V1 + V2) + groupid, family = "multinomial2", data = df.D, verbose = FALSE, prior=priors3, nitt = 200000, burnin = 5000, thin = 100)

saveRDS(mcmcfit4, "../output/analysis_groups_plot/data/mcmcfit4_group_aso_sexMF_gs_multinomial.rdata")

#Model 4.2: ~ V12sex + group size (exp. prior, FF ref)----
priors4<- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V =1, nu = 0.002), G2 = list(V=1, nu=1, alpha.mu=0, alpha.V=1000)))

mcmcfit4.2 <- MCMCglmm(cbind(aso.sucx, aso.fail) ~ V12sexF + gs, random = ~ mm(V1 + V2) + groupid, family = "multinomial2", data = df.D, verbose = FALSE, prior=priors4, nitt = 200000, burnin = 5000, thin = 100)

saveRDS(mcmcfit4.2, "../output/analysis_groups_plot/data/mcmcfit4.2_group_aso_sexF_gs_multinomial.rdata")

#Model 5: ~ V12sex + group size + kinship (exp. prior, MF ref)----
priors5<- list(R = list(V = 1, nu = 0.002), G = list(G1 = list(V =1, nu = 0.002), G2 = list(V=1, nu=1, alpha.mu=0, alpha.V=1000)))

mcmcfit5 <- MCMCglmm(cbind(aso.sucx, aso.fail) ~ V12sex + gs + r, random = ~ mm(V1 + V2) + groupid, family = "multinomial2", data = df.D, verbose = FALSE, prior=priors5, nitt = 200000, burnin = 5000, thin = 100)

saveRDS(mcmcfit5, "../output/analysis_groups_plot/data/mcmcfit5_group_aso_sexMF_gs&r_multinomial.rdata")

#----check model area------
#check if it's better to include gs or not
mcmcfit$DIC  #17130
mcmcfit3$DIC #17129, not much difference...
mcmcfit4$DIC #17128, with expanded prior, gs; a bit better
mcmcfit5$DIC #17114

#Final model [mcmcfit4.2]----
fit <- mcmcfit4.2
vif.MCMCglmm(fit)
autocorr.diag(fit$VCV)  #model4 is doing better
autocorr.diag(fit$Sol)
dev.new(width=100, heigth=500, unit="px")
plot(fit$VCV)
plot(fit$Sol)

gelman.diag(mcmcfit3$Sol, mcmcfit3.2$Sol) #this needs two chains to check the mixing

#summary
summary(fit)
summary(fit)$solutions

#work with emmeans
library(emmeans)
library(tidybayes)
library(dplyr)
library(ggplot2)


#PLOT(it worked!)
mcmcfit4.2 %>% emmeans(~ V12sexF, data = df.D) %>% 
  contrast(method = "pairwise") %>% 
  gather_emmeans_draws() %>% #{tidybayes}
  ggplot(aes(x = .value, y = contrast)) +
  geom_halfeyeh()

#------------STATS--------------------
#"Daytime groups were larger than nighttime groups"----
library(glmmTMB)
source("overdisp_fun.R")

df.g <- rbind(Dgroup_unit, Ngroup_unit) %>% mutate(gtype=factor(gtype, levels=c("sleeping", "daytime"))) %>% setDT()

#poisson, overdispersion
fit0 <- glmmTMB(gs ~ gtype*year + (1|plot), family = poisson,  data=df.g)
overdisp_fun(fit0) #ratio=1.06, p=0.15
glmmTMB:::Anova.glmmTMB(fit0)
summary(fit0)

#no interaction term
fit <- glmmTMB(gs ~ gtype + year + (1|plot), family = poisson,  data=df.g)
#diagnostic: 
overdisp_fun(fit) #ratio=1.07, p=0.1

summary(fit)
glmmTMB:::Anova.glmmTMB(fit) #there is year difference

emm.fit <- emmeans(fit, pairwise ~ gtype + year, type = "response")
emm.fit
plot(emm.fit)

#------------Backdrop------------------
#Is sleepingB and sleepingNB same?(not done)----
sleeping_BNB <- mygroup_w %>% 
  filter(!is.na(sleepingB) & !is.na(sleepingNB))



  
