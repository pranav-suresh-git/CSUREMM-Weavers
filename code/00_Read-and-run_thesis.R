source("00_import_logger.R") #if you need to update logger data


## File structure-----------

# Data: where you find original data (.xlsx) and MS access (.accdb) 

# R >
#   > code:


#     Chap1 finding pairs        #need to copy files from finding pairs_v2
#     Chap1 finding colony members #also coding to summarise colony size
                                 #adapted from finding_colony_member.Rmd
#     Chap1_import and tidy data #import and tidy data
#     Chap1 association null model
#     Chap1 finding groups.R     #daytime, nighttime gmm, cluster analysis
#**   Chap1 group analysis.R     #analyze group size etc.
                                 #define soical units(NF, EF etc)
                                 #aso between mated ind.(dyad links)
#     Chap1 raso and social levels  #summary for association, r for pair, family and colony
#     Chap1 yearplot association and r.R 
                                 #output PLOT level dyadic association and r
#     Chap1 analysis r and association 
                                 #MRQAP + meta-analysis (final model)
                                 #adapted from anlaysis_r_association_ss.rmd
                                 #adapted from analysis_r_association_metanalyais.rmd

#     Chap1 kin nw               #calcualte kin nw table
#     Chap1 plot your group      #plot your nw based on hc group results(!!success!!)
                                 #plot association and kinship nw together
                                 #Fig.4
#     Chap1 dyad re-association  #re-pair rate
                                 #modified from analysis_pair_bond.r

#     Chap1 analysis r and association_storage 
                                 #adapted from anlaysis_r_association_ss.rmd
                                 #MRQAP + meta-analysis
                                 #different MRQAP model

#     Chap2 import and tidy data.R 
                                 #tidy rainfall data
#     Chap2 coding               #Part1: colony dynamics (tidy colony status)
                                 #Part2: dyadic movement
#     Chap2 exploratory          #analyze FF dyads in relation to rainfall
#     Chap2 temporal dynamics raso 
                                 #calculate r and association throughout years
#     Chap2 analysis_dynamics.rmd 
                                 #individual movement
                                 #chi-square test on FFD at colony level
#     Chap2 analysis_dynamics_MCMCglmm.rmd 
                                 #FFD, MCMCglmm
#     Chap2 group level analysis.rmd #group level analysis





#-----function code-----------------------------------------
#     fun_add_triangle.R         #add triangle shape in igraph
#     fun_r.R                    #fill relatedness(fun_r); 
#                                #fill numeric data for dyads(fun_r2)
#     fun_weight.R               #fill numeric value for any dyads
#     fun_kinship.R              #fill kinship
#     fun_sex.R                  #fun_sex: find sex
#                                #fun_sexdyad: find sex and sexcombo for edgelist
#     fun_N_net.R                #output adjacent matrix and igraph object
#     fun_D_net.R
#     fun_membership.R           #output membership and community object
#     fun_rcomm. R               #bootstrap to calculate network robusteness 
#     funs_plotnw.R              #fun_attr, fun_plotnw, fun_plotcom, fun_plotmg, 
#                                #fun_plotnbi, fun_plotgs, fun_plotkin
#     fun_calc_rc_par.R          #function to bootstrap and find community(adapted from                                     #Damien's code)
     
#------------other relevant coding outside folder-----------------
#../relatedness/cervus/R/code/Filter_ML_kinship.R   #kinship criteria
#../relatedness/kingroup2                           #create pedigree
#../analysis/colony dynamics                        #colony locations across years




#
#   > data: data used in the code
#     2017 weaver all colony_1min_20181227_final.xlsx
#     2012-2017_weaver_attribute
#     2012-2017_weaver_relatedness
#     2012-2017_weaver_nestsite
#     2012-2017 weaver nest attribute
#     2013-2017 weaver nest dist_matrix.RData   #nest distance


