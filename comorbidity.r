# comorbidities table

# start with KPE+NF
library(dplyr)
dfKPEi <- read.csv('/home/or/rewardPTSD/KPEIHR0009_DATA_2021-03-16_1114.csv')
dfKPEi$scr_id <- as.factor(dfKPEi$scr_id)
varNames <- c('scr_id', 'scid_bid_current', 'scid_bid2_current', 'scid_mdd_lifetime', 'scid_mdd_current', 'scid_dysd_lifetime', 'scid_ddnos_pm',
              'scid5_alcohol_pm', 'scid5_sedative_pm', 'scid5_cannabis_pm', 'scid5_stimulants_pm', 'scid5_opioid_pm', 'scid5_cocaine_pm',
              'scid5_halpcp_pm', 'scid5_poly_pm', 'scid5_other_pm', 'scid_pd_pm', 'scid_awopd_pm', 'scid_socialp_pm', 'scid_specp_pm', 
              'scid_ocd_pm', 'scid_gad_lifetime', 'scid_adnos_pm', 'scid_somad_lifetime', 'scid_pain_lifetime', 'scid_hypoch_lifetime',
              'scid_bodydis_lifetime', 'scid_anorex_pm', 'scid_bulim_pm', 'scid_binge_pm')

CombKPE <- dfKPEi %>% filter(redcap_event_name == 'screening_clinicia_arm_1') %>% drop_na(scid_mdd_lifetime) %>% dplyr::select(any_of(varNames))
# remove the KPE from subject name
CombKPE$scr_id<-gsub("KPE","",as.factor(CombKPE$scr_id))
# remove spaces
CombKPE$scr_id<-gsub(" ","",as.factor(CombKPE$scr_id))

dfNFi <- read_csv('/home/or/rewardPTSD/NeurofeedbackIHR008_DATA_2021-03-16_1113.csv')
dfNFi$scr_id <- as.factor(dfNFi$scr_id)
CombNF <- dfNFi %>% filter(redcap_event_name == 'screening_arm_1') %>% drop_na(scid_mdd_lifetime) %>% dplyr::select(any_of(varNames))
# remove the NF from subject name
CombNF$scr_id<-gsub("NF","",as.factor(CombNF$scr_id))
# remove spaces
CombNF$scr_id<-gsub(" ","",as.factor(CombNF$scr_id))

dfCom <- merge(CombKPE, CombNF, by='scr_id') # get list of subjects apearing in both datasets
subs <- dfCom$scr_id
subs <- pull(dfCom, scr_id)
str(subs)
# remove it from the KPE data (subject rows are: 7, 31, 65, 69, 87,89,96)
CombKPE <- CombKPE[-c( 7, 31, 65, 69, 87,89,96), ]
# combine both data frames

dfCom <- rbind(CombKPE, CombNF)

# now summaries comorbidities
require(gtsummary)
table1 <- tbl_summary(dplyr::select(dfCom, -scr_id), missing='no')
table1

## Study 2
dfRCF <- read.csv('/home/or/rewardPTSD/ReconsolidationOfFea_DATA_2021-03-02_1737.csv')

varNames2 <- c('subject_id', 'scid_bid_current', 'scid_bid2_current', 'scid_mdd_lifetime', 'scid_mdd_current', 'scid_dysd_lifetime', 'scid_ddnos_pm',
              'scid_alcohol_pm', 'scid_sedative_pm', 'scid_cannabis_pm', 'scid_stimulants_pm', 'scid_opioid_pm', 'scid_cocaine_pm',
              'scid_halpcp_pm', 'scid_poly_pm', 'scid_other_pm', 'scid_pd_pm', 'scid_awopd_pm', 'scid_socialp_pm', 'scid_specp_pm', 
              'scid_ocd_pm', 'scid_gad_lifetime', 'scid_adnos_pm', 'scid_somad_lifetime', 'scid_pain_lifetime', 'scid_hypoch_lifetime',
              'scid_bodydis_lifetime', 'scid_anorex_pm', 'scid_bulim_pm', 'scid_binge_pm')

comRCF <- dfRCF %>% filter(redcap_event_name == 'screening_visit_arm_1') %>% drop_na(scid_mdd_lifetime) %>% dplyr::select(any_of(varNames2))
table2 <- tbl_summary(dplyr::select(comRCF, -subject_id), missing='no')
table2
