###############
#Libraries#####
###############
library(tidyverse)
library(openxlsx)
library(readr)

#Function
source("2_programs/functions.R")

#Paths
path_data = paste0(getwd(),"/1_data")

###################
#A. Read Data  ####
###################
#Read Risk factors
df_riskfac <- read_csv("1_data/final/seha_comorbidity_ext_mar_dec_2020.csv")
df_riskfac <- df_riskfac %>% group_by(PERS_ID) %>% slice(1) %>% ungroup() 
df_riskfac <- df_riskfac %>% 
  tidyr::extract(AGE, c('age','age_mea'), regex = "(.*)\\s(.*)", remove = FALSE) %>% 
  filter(age_mea %in% c('Years') & as.numeric(age)>=18) 

#Read PCR Tests
fls_nms <- list.files(paste0(path_data,'/final'), pattern = ".*_\\d.csv") %>% setNames(.,nm = gsub('\\D+','',.))

df0 <- map_df(fls_nms,
               ~read_csv(paste0(path_data,'/final/',.x),
                         col_types = cols(DATE_COLLECTED = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
                                          RESULT_PERFORMED_DT = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
                                          RESULT_VERIFY_DATE = col_datetime(format = "%m/%d/%Y %H:%M:%S"))) ,
               .id = "part")

ids_missing <- base::setdiff(df_riskfac$PERS_ID, df0$PERSON_ID)

df0 <- df0 %>% 
  mutate(RESULT_num = case_when(RESULT %in% c('2019-nCoV NOT DETECTED', 'NEG', 'Not Detected') ~ 0,
                                RESULT %in% c('2019-nCoV DETECTED') ~ 1,
                                TRUE ~ NA_real_),
         ASSAY_num = case_when(ASSAY == "CoViD19 RT-PCR" ~ 1,
                               ASSAY == "CoVID-19 Rapid PCR" ~ 2,
                               ASSAY == "MDx MERS Coronavirus RT-PCR" ~ 3,
                               TRUE ~ NA_real_)) %>% 
  group_by(PERSON_ID) %>% 
  arrange(DATE_COLLECTED, .by_group=TRUE) %>% ungroup()

#Choose only ASSAY is CoViD19 RT-PCR and non-missing RESULT_num & DATE_COLLECTED $ number of visitis >= 3
df1 <- df0 %>% filter(ASSAY_num==1 & !is.na(RESULT_num) & !is.na(DATE_COLLECTED)) %>% 
  distinct(PERSON_ID, DATE_COLLECTED, .keep_all = TRUE) 

df2 <- df1 %>% group_by(PERSON_ID) %>% mutate(flag=cumsum(RESULT_num==1)) %>% 
  filter(flag>=1) %>% 
  filter(n()>=3) %>%
  mutate(flag_seq=paste0(rle(RESULT_num)$values, collapse = ""),
         patterns1=case_when(flag_seq %in% c('1','10') ~ 'Normal',
                            TRUE ~ 'Abnormal'),
         patterns2=case_when(patterns1=='Abnormal' & any(rle(RESULT_num)$lengths[rle(RESULT_num)$values==0 & lead(rle(RESULT_num)$values)==1]>=2) ~ 'Abnormal',
                             TRUE ~ 'Normal')) %>% 
  ungroup() 

df2_ep1 <- group_by(df2, PERSON_ID) %>% slice(1)

# df3 <- df2 %>% filter(patterns == "Abnormal") %>% group_by(PERSON_ID) %>% 
#   group_modify(~cal_timediff(df_r = .x)) %>% 
#   ungroup()

df3 <- df2_ep1 %>% inner_join(df_riskfac, by=c('PERSON_ID'='PERS_ID'))

############################
#Write data to disk
###########################
#PCR Tests
write.csv(df2,file = "1_data/df_long_fullPCRtests.csv", row.names = FALSE)
write.csv(df2_ep1,file = "1_data/df_ep1_fullPCRtests.csv", row.names = FALSE)

#PCR Tests + Risk factors
write.csv(df3,file = "1_data/df_ep1.csv", row.names = FALSE)

