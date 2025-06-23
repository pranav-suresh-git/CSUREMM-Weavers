#Find closest associate (heterosexual, non-first order relatives)
aso_mf <- aso %>% 
  filter(V12sex == "mf") %>% 
  mutate(vm = ifelse(V1sex=="m", id1, id2), #rearrange V1 male and V2 female
         vf = ifelse(V2sex=="f", id2, id1)) %>% 
  mutate(id1=vm, id2=vf) %>% 
  select(-vm, -vf, -V1sex, -V2sex)

asoD_mf <- aso_mf %>% filter(gtype == "daytime") %>% select(id1, id2, year, plot, V12sex, association, kinorder)
asoN_mf <- aso_mf %>% filter(gtype == "sleeping") %>% select(id1, id2, year, plot, V12sex, association, kinorder)

#[asoD_mf_ur]----
#this is arranged by mf
asoD_mf_ur <- asoD_mf %>% filter(kinorder!="first" & association >0)

#male highest aso
m_highaso <- asoD_mf_ur %>% 
  group_by(year, plot, id1) %>% 
  arrange(-association, .by_group=T) %>% slice(1) %>% 
  setDT()

#female highest aso
f_highaso <- asoD_mf_ur %>% 
  group_by(year, plot, id2) %>% 
  arrange(-association, .by_group=T) %>% slice(1) %>% 
  setDT()                       

#[asoD_highest]----
asoD_highest <- rbind(m_highaso, f_highaso) %>% 
  left_join(select(uid_combo, uid, combo), by = c("id1" = "combo")) %>% 
  left_join(select(uid_combo, uid, combo), by = c("id2" = "combo")) %>%
  mutate(pairid = paste(uid.x, uid.y, sep = ".")) %>%   #add $pairid
  select(-uid.x, -uid.y) %>%
  group_by(year, plot, id1) %>% 
  group_by(year, plot, pairid) %>% 
  mutate(n_pairid = n()) %>%   # n=2 if inds are closest association with each other
  distinct() %>% 
  setDT()

#[pair_B][pair_NB]

#-------------Finding pairs-------------------
###############################
#First pass : breed together
###############################
sum_Bvisit_bynest <- daytime %>%
  left_join(select(BD, year, nest, nestid, date), by=c("year", "nest", "date")) %>% 
  filter(!is.na(nestid)) %>% 
  filter(color != "MYPO") %>% #no sex
  count(year, plot, colony, nest, nestid, color) %>% 
  mutate(sex = nestbird_attr[match(color, nestbird_attr$node)]$sex) %>% 
  group_by(year, plot, nest, nestid) %>%  #by nestid
  arrange(desc(n), .by_group = T) %>% 
  mutate(rn = row_number(), visit = n) %>% 
  add_count(year, plot, colony, nest) %>% rename(n_birds = n)
  
B_owner4 <- sum_Bvisit_bynest %>% filter(n_birds > 1 & rn <= 4) %>% mutate(sexn = dplyr::recode(sex, "m" = 1, "f" = 0))

#create combination for top 3 nest owners
comb_B_owner4 <- B_owner4 %>% 
  group_by(year, plot, colony, nest, nestid) %>% 
  group_modify(., ~ as.data.frame.matrix(t(combn(.$color, 2)))) %>% #create combo
  as.data.table() %>% 
  mutate(V1sex = fun_sexdyad(.[,.(V1,V2)], sex)[[1]],
         V2sex = fun_sexdyad(.[,.(V1,V2)], sex)[[2]],
         V12sex = fun_sexdyad(.[,.(V1,V2)], sex)[[3]]) %>% 
  filter(V12sex == "mf") %>% 
  mutate(vm = ifelse(V1sex=="m", V1, V2), #rearrange V1 male and V2 female
         vf = ifelse(V2sex=="f", V2, V1)) %>% 
  mutate(V1=vm, V2=vf) %>% 
  select(-vm, -vf, -V1sex, -V2sex) %>% 
  left_join(select(B_owner4, year, plot, colony, nest, nestid, color, rn), 
            by = c("year", "plot", "colony", "nest", "nestid", "V1" = "color")) %>% 
  left_join(select(B_owner4, year, plot, colony, nest, nestid, color, rn), 
            by = c("year", "plot", "colony", "nest", "nestid", "V2" = "color"), suffix = c("V1", "V2")) %>% 
  mutate(sumrn = rnV1 + rnV2) %>% 
  left_join(select(asoD_mf, year, plot, id1, id2, association),
            by=c("year", "plot", "V1"="id1", "V2"="id2")) %>%
  left_join(select(asoN_mf, year, plot, id1, id2, association),
            by=c("year", "plot", "V1"="id1", "V2"="id2"), suffix=c(".D", ".N")) %>% 
  as.data.table() %>% 
  mutate(r = fun_r(.[,.(V1,V2)], relate), 
         kinship = fun_kinship(.[,.(V1,V2)], kinship, "kinorder")) %>% 
  setDT()

#[pair_B]----
#pairs breeding together
pair_B <- comb_B_owner4 %>% 
  filter(kinship!="first" & association.D >0 ) %>% 
  group_by(year, nest, nestid) %>% #by nestid 
  arrange(sumrn, .by_group = T) %>% 
  slice(1) %>% #first pair for each nestid
  left_join(select(uid_combo, uid, combo), by = c("V1" = "combo")) %>% 
  left_join(select(uid_combo, uid, combo), by = c("V2" = "combo"), suffix=c("1","2")) %>% 
  mutate(pairid = paste0(uid1, ".", uid2)) %>% 
  select(-uid1, -uid2) %>% 
  mutate(source = "B") %>% 
  setDT()


output_pairB <- pair_B %>% select(year, plot, colony, nest, nestid, V1, V2, V12sex, r, kinship, pairid)

#write_xlsx(output_pairB, "../output/finding pair/output_2013-2017 weaver pair by nestid.xlsx")

#########################################
#2nd pass: NB highest associated with the same nests
#########################################
sum_NBvisit_bynest <- daytime %>%
  left_join(select(BD, year, nest, nestid, date), by=c("year", "nest", "date")) %>% 
  filter(is.na(nestid)) %>% 
  filter(color != "MYPO") %>% #no sex
  count(year, plot, colony, nest, color) %>% 
  mutate(sex = nestbird_attr[match(color, nestbird_attr$node)]$sex) %>% 
  group_by(year, plot, nest) %>% 
  arrange(desc(n), .by_group = T) %>% 
  mutate(rn = row_number(), visit = n) %>% 
  add_count(year, plot, colony, nest) %>% rename(n_birds = n)

NB_owner4 <- sum_NBvisit_bynest %>% filter(n_birds > 1 & rn <= 4) %>% mutate(sexn = dplyr::recode(sex, "m" = 1, "f" = 0))

#create combination for top 3 nest owners
comb_NB_owner4 <- NB_owner4 %>% 
  group_by(year, plot, colony, nest) %>% 
  group_modify(., ~ as.data.frame.matrix(t(combn(.$color, 2)))) %>% #create combo
  as.data.table() %>% 
  mutate(V1sex = fun_sexdyad(.[,.(V1,V2)], sex)[[1]],
         V2sex = fun_sexdyad(.[,.(V1,V2)], sex)[[2]],
         V12sex = fun_sexdyad(.[,.(V1,V2)], sex)[[3]]) %>% 
  filter(V12sex == "mf") %>%    #rearrange mf
  mutate(vm = ifelse(V1sex=="m", V1, V2), 
         vf = ifelse(V2sex=="f", V2, V1)) %>% 
  mutate(V1=vm, V2=vf) %>% 
  select(-vm, -vf, -V1sex, -V2sex) %>% #rearrange V1 male and V2 female
  left_join(select(NB_owner4, year, plot, colony, nest, color, rn), 
            by = c("year", "plot", "colony", "nest", "V1" = "color")) %>% 
  left_join(select(NB_owner4, year, plot, colony, nest, color, rn), 
            by = c("year", "plot", "colony", "nest", "V2" = "color"), suffix = c("V1", "V2")) %>% 
  mutate(sumrn = rnV1 + rnV2) %>%
  left_join(select(asoD_mf, year, plot, id1, id2, association),
            by=c("year", "plot", "V1"="id1", "V2"="id2")) %>%
  left_join(select(asoN_mf, year, plot, id1, id2, association),
            by=c("year", "plot", "V1"="id1", "V2"="id2"), suffix=c(".D", ".N")) %>%
  as.data.table() %>% 
  mutate(r = fun_r(.[,.(V1,V2)], relate), 
         kinship = fun_kinship(.[,.(V1,V2)], kinship, "kinorder")) %>% 
  setDT()

#[pair_NB]----
pair_NB <- comb_NB_owner4 %>% 
  filter(kinship!="first" & association.D >0) %>%
  left_join(select(asoD_highest, year, plot, id1, id2, pairid, n_pairid),
            by=c("year", "plot", "V1"="id1", "V2"="id2")) %>%
  mutate(n_pairid = replace_na(n_pairid, 0)) %>% #neigher is highest associate
  filter(n_pairid == 2) %>% #V1 and V2 has to be closet associate with each other
  select(-n_pairid) %>% #but they don't have to be the dyads with highest assoc. with the nest
  mutate(source = "NB") %>% 
  setDT()
  
#[pair_BNB]----
pair_BNB <- bind_rows(pair_B[, "nestid":=NULL], pair_NB) %>% distinct()
  
#[pair_final_bynest]----
pair_BNB_bynest <- pair_BNB %>% 
  select(-source) %>% 
  distinct()

#[pair_final]----
pair_final <- pair_BNB_bynest %>% 
  select(year, plot, V1, V2, V12sex, association.D, association.N, r, kinship, pairid) %>% 
  distinct() %>% 
  arrange(year, V1, V2)

compare_ss <- pair_final %>% 
  full_join(select(pair_mf, year, plot, V1, V2, V12sex, pairid), by = c("year", "plot", "V1", "V2"))

#write_xlsx(compare_ss, "../output/finding pair/output_compare old and new coding_ss.xlsx")

#Check males with > 1pairs in the outputfile (with criteria) 
#Final pair file is saved as "2013-2017 weaver pair_FINAL.xlsx"

#Tidy pair data
pair_mf <- read_xlsx("../output/finding pair/2013-2017 weaver pair_final.xlsx")
pair_fm <- pair_mf %>% rename(V1=V2, V2=V1) 

pair <- bind_rows(pair_mf, pair_fm) %>% 
  mutate(V1sex = fun_sex(V1, sex), V2sex = fun_sex(V2, sex))

#write_xlsx(pair, "../output/finding pair/output_2013-2017 weaver pair_final_repeated.xlsx")

