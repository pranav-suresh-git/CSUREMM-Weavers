#Aim:find colony members and give summary stats.
#try two different stats: 
#1)[colony_membmer]: >1night 
#2)[colony_member_main]: only the main colonies where they spent the most nights (15% ind.moved within the season)
#3)[colony_member_month]:>1 night and give membership for each month


#[colony_member]-----
#def: stay > 1night
colony_member <- sleepnest %>% 
  count(year, color, plot, colony) %>% 
  rename(n_night_colony = n) %>% 
  arrange(year, color, plot, -n_night_colony) %>% 
  group_by(year, color) %>% mutate(n_colony = n()) %>% 
  filter(n_night_colony > 1) %>% #sleep at the colony >1 night 
  select(-n_colony) %>% 
  group_by(year, plot, color) %>% 
  mutate(n_colony = n()) %>% 
  as.data.table() %>% 
  mutate(sex = fun_sex(.$color, sex)) %>%
  left_join(select(age, year, color, age, season_nstl), by = c("year", "color")) %>% #add $age, pass
  group_by(year, color) %>% 
  mutate(max_nights = max(n_night_colony)) %>% 
  group_by(year, color) %>% 
  mutate(main_colony = ifelse(n_night_colony == max_nights, 1, 0)) %>% 
  setDT()

#write_xlsx(colony_member, paste0("../output/colony member/data/output_colony_member.xlsx"))


#summary by colony
n_colonymember <- colony_member %>% 
  group_by(year, plot, colony) %>% 
  summarise(n_birds = n()) %>% 
  ungroup()

#write_xlsx(n_colonymember, "../output/colony member/data/output_n_colony_member_2nights.xlsx")

#summary by yearplot
sum_meansd_yearplot <- n_colonymember %>% 
  group_by(year, plot) %>% 
  summarise(n = n(), mean = mean(n_birds), sd = sd(n_birds), max = max(n_birds))

#write_xlsx(sum_meansd_yearplot, "../output/colony member/data/output_colony_member_2nights_meansd_yearplot.xlsx")

#total meansd
sum_meansd <- n_colonymember %>% 
  summarise(mean = mean(n_birds), sd = sd(n_birds))

#write_xlsx(sum_meansd_yearplot, "../output/colony member/data/output_colony_member_2nights_meansd.xlsx")

#--------colony member MAIN-------
#[colony_member_main]----
colony_member_main <- colony_member %>% filter(main_colony==1) %>% setDT()

#[colony_member_month]----
colony_member_month <- sleepnest %>% 
  mutate(month = month(sleep_date)) %>% 
  count(year, plot, color, month, colony) %>% 
  group_by(year, plot, color, month) %>% 
  arrange(-n,.by_group = T) %>% 
  slice(1) %>% #use the most spent nights for the month
  group_by(year, color) %>%
  filter(row_number()==1 | row_number()==n()) %>% 
  mutate(rn = row_number()) %>% 
  select(-month, -n) %>% 
  pivot_wider(., names_from = "rn", values_from = "colony") %>% 
  rename("colony_early"="1", "colony_late"="2") %>% 
  mutate(colony_late = ifelse(is.na(colony_late), colony_early, colony_late)) %>% 
  mutate(samecolony = ifelse(colony_early==colony_late, 1, 0))  #same colony in the early and late season

#write_xlsx(colony_member_month, paste0("../output/colony member/data/output_colony_member_month.xlsx"))

##[colony_member_month_l]----
colony_member_month_l <- colony_member_month %>% 
  rename("early" = "colony_early", "late" = "colony_late") %>% 
  select(-samecolony) %>% 
  pivot_longer(., cols = c("early", "late"), names_to = "season", values_to="colony") %>% 
  setDT()

#[colony_member_main1_1night]----
#this contains ind that slept only one night
#colony choice: most nights -> early season
colony_member_main1_1night <- sleepnest %>% 
  count(year, color, plot, colony) %>% 
  rename(n_night_colony = n) %>% 
  arrange(year, color, plot, -n_night_colony) %>% 
  group_by(year, color) %>% mutate(n_colony = n()) %>% 
  select(-n_colony) %>% 
  group_by(year, plot, color) %>% 
  mutate(n_colony = n()) %>% 
  as.data.table() %>% 
  mutate(sex = fun_sex(.$color, sex)) %>%
  left_join(select(age, year, color, age, season_nstl), by = c("year", "color")) %>% #add $age, pass
  group_by(year, color) %>% 
  mutate(max_nights = max(n_night_colony)) %>% 
  group_by(year, color) %>% 
  mutate(main_colony = ifelse(n_night_colony == max_nights, 1, 0)) %>% 
  filter(main_colony==1) %>% #n=1047
  select(-n_night_colony, -n_colony, -season_nstl, -max_nights, -main_colony) %>% 
  group_by(year, color) %>% 
  mutate(n_colony = n()) %>% 
  left_join(colony_member_month_l[season=="early",], by = c("year", "plot", "color", "colony")) %>% 
  filter(n_colony==1 |(n_colony==2 & season=="early")|(n_colony==3 & season=="late")) %>% 
  setDT()

#write_xlsx(colony_member_main1_1night, paste0("../output/colony member/data/output_colony_member_main1_1night.xlsx"))


#---------
n_colonymember_main <- colony_member_main %>% 
  group_by(year, plot, colony) %>% 
  summarise(n_birds = n()) %>% 
  ungroup()

sum_main_meansd_yearplot <- n_colonymember_main %>% 
  group_by(year, plot) %>% 
  summarise(n = n(), mean = mean(n_birds), sd = sd(n_birds), max = max(n_birds))

#-----colony size----
#[colony_size]----
colony_size <- colony_member %>% count(year, plot, colony) %>% rename(n_birds = n) %>% full_join(sum_n_nest) %>% rename(n_nests = n) %>% mutate(n_birds = replace_na(.$n_birds, 0)) %>% setDT()
##previous coding didn't include colony with no birds sleeping

#write_xlsx(colony_size, paste0("../output/colony member/data/output_colony_size.xlsx"))

#[sum_colony_size]----
sum_colony_size <- colony_size %>% 
  summarise(mean_n_nest = mean(n_nests), sd_n_nest = sd(n_nests), n = n(),
            mean_n_bird = mean(n_birds), sd_n_bird = sd(n_birds), n = n())

view(sum_colony_size)

#-------n of sleep colonies-------
night_sleep_colony <- sleepnest %>% count(year, color, colony) %>% rename(n_night_colony = n) %>% arrange(year, color, -n_night_colony) %>% group_by(year, color) %>% mutate(total_nights = sum(n_night_colony)) %>% ungroup()

# no. of sleeping colony[sum_sleep_colony]----
sum_sleep_colony <- night_sleep_colony %>% count(year, color) %>% rename(n_colony = n) %>% count(year,  n_colony) %>% group_by(year) %>%  mutate(total_birds = sum(n), per = round(n/total_birds, digits = 2))

sum_sleep_2colony_year <- sum_sleep_colony %>% mutate(onecolony = ifelse(n_colony ==1, 1, 0)) %>% group_by(year, onecolony) %>% summarise(sum = sum(per))

sum_sleep_2colony <- sum_sleep_2colony_year %>% group_by(onecolony) %>% summarise(mean = mean(sum), sd = sd(sum)) %>% print()
