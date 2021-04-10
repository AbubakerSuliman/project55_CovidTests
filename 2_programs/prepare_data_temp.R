rm(list = ls())
###############
#Libraries#####
###############
library(tidyverse)
library(openxlsx)
library(readr)
library(lubridate)
library(rollmatch)

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

#df0 = df0 %>% group_by(PERSON_ID,DATE_COLLECTED) %>% slice(1) 

#assign random treatment for each person#

sub = df0 %>% pull(PERSON_ID) %>% unique()
sub_treat1 = sample(sub,length(sub)/2,replace=FALSE)
sub_treat0 = setdiff(sub,sub_treat1)
df1 = df0 %>% mutate(treat = if_else(PERSON_ID %in% sub_treat1, 1,0))

#Initial data to be received#
df1.1 = df1 %>% filter(treat==1)
df1.2 = df1.1 %>% group_by(PERSON_ID)  %>%  summarise(dose1_date = as.Date(median(DATE_COLLECTED))) %>% filter(!is.na(dose1_date))
df0.1 = df1 %>% filter(treat==0)

#Clean the data first#

df1.1 = df1.1 %>% mutate(DATE_COLLECTED = as.Date(DATE_COLLECTED)) %>% filter(year(DATE_COLLECTED) >= 2020)
df0.1 = df0.1 %>% mutate(DATE_COLLECTED = as.Date(DATE_COLLECTED)) %>% filter(year(DATE_COLLECTED) >= 2020)
df1.2 = df1.2 %>% mutate(dose1_date = as.Date(dose1_date)) %>% filter(dose1_date > ymd("2020-01-01"))

#Choose only ASSAY is CoViD19 RT-PCR and non-missing RESULT_num & DATE_COLLECTED
df1.1 <- df1.1 %>% filter(ASSAY_num==1 & !is.na(RESULT_num) & !is.na(DATE_COLLECTED)) %>% distinct(PERSON_ID, DATE_COLLECTED, .keep_all = TRUE) 
df0.1 <- df0.1 %>% filter(ASSAY_num==1 & !is.na(RESULT_num) & !is.na(DATE_COLLECTED)) %>% distinct(PERSON_ID, DATE_COLLECTED, .keep_all = TRUE) 

#Add entry date for treated#
df1.1 = df1.1 %>% left_join(df1.2,  c("PERSON_ID"="PERSON_ID"))
#merge treated with controls#
df1  = bind_rows(df1.1, df0.1)

#create the time period for all#
min_vaccine_date = as.Date(min(na.omit(df1[df1$treat==1,]$dose1_date)) )
df1 = df1 %>% mutate(time0 = 1 + round(as.numeric(as.character(difftime(as.Date(DATE_COLLECTED), as.Date(min_vaccine_date), units = "days"))))  )

#check there is no duplicate time0 per ID#

sub = df1  %>% group_by(PERSON_ID, time0) %>% filter(n() > 1)

#create entry time for all#
df2 = df1 %>% group_by(PERSON_ID) %>% mutate(entry_time = if_else(treat==1, 1 + round(as.numeric(as.character(difftime(dose1_date, min_vaccine_date, units = "days")))), 2 + round(as.numeric(as.character(difftime(DATE_COLLECTED, min_vaccine_date, units = "days")))) )) %>% ungroup()

df3 = df2 %>% filter(time0 > 0 & !is.na(entry_time) & entry_time > 0)

#First matching#
library(rollmatch)
reduced_data <- rollmatch::reduce_data(data = df3, treat = "treat",tm = "time0", entry = "entry_time",id = "PERSON_ID", lookback = 1)
fm <- as.formula(treat ~ RESULT_num)
vars <- all.vars(fm)

scored_data <- score_data(reduced_data = reduced_data,
                          model_type = "logistic", match_on = "logit",
                          fm = fm, treat = "treat",
                          tm = "time0", entry = "entry_time", id = "PERSON_ID")


output <- rollmatch(scored_data, data=df3, treat = "treat",
                    tm = "time0", entry = "entry_time", id = "PERSON_ID",
                    vars = vars, lookback = 1, alpha = .2,
                    standard_deviation = "average", num_matches = 1,
                    replacement = FALSE)









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