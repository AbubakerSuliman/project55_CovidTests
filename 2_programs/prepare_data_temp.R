###############
#Libraries#####
###############
library(tidyverse)
library(openxlsx)
library(readr)

#Function
source("2_programs/functions.R")

###################
#A. Read Data  ####
###################
df0 <- read_csv("1_data/covid_tests_Imran.csv", 
                     col_types = cols(DATE_COLLECTED = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                      RESULT_PERFORMED_DT = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                      RESULT_VERIFY_DATE = col_datetime(format = "%m/%d/%Y %H:%M")))

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

#Remove duplicate PCR according to date collected #

df0 = df0 %>% group_by(PERSON_ID,DATE_COLLECTED) %>% slice(1) 


#assign random treatment for each person#

sub = df0 %>% pull(PERSON_ID) %>% unique()
sub_treat1 = sample(sub,length(sub)/2,replace=FALSE)
sub_treat0 = setdiff(sub,sub_treat1)

df1 = df0 %>% mutate(treat = if_else(PERSON_ID %in% sub_treat1, 1,0))

#create the time period#
min_date = min(na.omit(df1$DATE_COLLECTED)) 
df1 = df1 %>% mutate(time0 = as.numeric(as.character(difftime(as.Date(DATE_COLLECTED), as.Date(min_date), units = "days")))  )
#create date of vaccine for treat#

#create entry period for treated and for controls#

df1.0 = df1 %>% group_by(treat, PERSON_ID) %>% 

#Create matching#


#Choose only ASSAY is CoViD19 RT-PCR and non-missing RESULT_num & DATE_COLLECTED
df1 <- df0 %>% filter(ASSAY_num==1 & !is.na(RESULT_num) & !is.na(DATE_COLLECTED)) %>% 
  distinct(PERSON_ID, DATE_COLLECTED, .keep_all = TRUE)

df2 <- df1 %>% group_by(PERSON_ID) %>% mutate(flag=cumsum(RESULT_num==1)) %>% 
  filter(flag>=1) %>% 
  mutate(flag_seq=paste0(rle(RESULT_num)$values, collapse = ""),
         patterns=case_when(flag_seq %in% c('1','10') ~ 'Normal',
                            TRUE ~ 'Abnormal')) %>% 
  ungroup() 

df3 <- df2 %>% filter(patterns == "Abnormal") %>% group_by(PERSON_ID) %>% 
  group_modify(~cal_timediff(df_r = .x)) %>% 
  ungroup()

df3_ep1 <- group_by(df3, PERSON_ID) %>% slice(1)


############################
#Write data to disk
###########################

write.csv(df3,file = "1_data/df_long.csv", row.names = FALSE)
write.csv(df3_ep1,file = "1_data/df_ep1.csv", row.names = FALSE)