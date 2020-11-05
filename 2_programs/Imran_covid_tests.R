library(tidyverse)
library(openxlsx)
library(readr)

ct_Imran <- read_csv("D:/covid_tests/covid_tests_Imran.csv", 
                     col_types = cols(DATE_COLLECTED = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                      RESULT_PERFORMED_DT = col_datetime(format = "%m/%d/%Y %H:%M"), 
                                      RESULT_VERIFY_DATE = col_datetime(format = "%m/%d/%Y %H:%M")))

ct_Imran <- ct_Imran %>% mutate(RESULT_def1 = case_when(RESULT %in% c('2019-nCoV NOT DETECTED', 'NEG', 'Not Detected') ~ 0,
                                           RESULT %in% c('2019-nCoV DETECTED', 'Presumptive Positive', 'Presumptive POSITIVE') ~ 1,
                                           TRUE ~ NA_real_),
                                RESULT_def2 = case_when(RESULT %in% c('2019-nCoV NOT DETECTED', 'NEG', 'Not Detected') ~ 0,
                                                        RESULT %in% c('2019-nCoV DETECTED') ~ 1,
                                                        TRUE ~ NA_real_),
                                ASSAY_num = case_when(ASSAY == "CoViD19 RT-PCR" ~ 1,
                                                      ASSAY == "CoVID-19 Rapid PCR" ~ 2,
                                                      ASSAY == "MDx MERS Coronavirus RT-PCR" ~ 3,
                                                      TRUE ~ NA_real_)) %>% 
  group_by(ENCNTR_ID) %>% arrange(DATE_COLLECTED, .by_group=TRUE) %>% ungroup()

####################################################
#Def 1: Code Presumptive Positive As 1 (Positive) ##
####################################################
result_PCR_def1 <- ct_Imran %>% group_by(ENCNTR_ID) %>% filter(n()>=3) %>% 
  summarise(PCR_seq = paste0(rle(RESULT_def1)$values, collapse = ""), 
            ASSAY_seq = paste0(rle(ASSAY_num)$values, collapse = "")) %>% 
  count(PCR_seq) %>% 
  mutate(`%`= sprintf('%0.2f%%',(n/sum(n)*100)))

result_ASSAY_PCR_def1 <- ct_Imran %>% group_by(ENCNTR_ID) %>% filter(n()>=3) %>% 
  summarise(PCR_seq = paste0(rle(RESULT_def1)$values, collapse = ""), 
            ASSAY_seq = paste0(rle(ASSAY_num)$values, collapse = "")) %>% 
  count(ASSAY_seq,PCR_seq) %>% 
  group_by(ASSAY_seq) %>% 
  mutate(`%`= sprintf('%0.2f%%',(n/sum(n)*100)))


lst_def1 <- list("PCR_All" = result_PCR_def1, "PCR_By_ASSAY" = result_ASSAY_PCR_def1)
write.xlsx(lst_def1, file="result_def1.xlsx", row.names=FALSE)

####################################################
#Def 2: Code Presumptive Positive As NA ############
####################################################
result_PCR_def2 <- ct_Imran %>% group_by(ENCNTR_ID) %>% filter(n()>=3) %>% 
  summarise(PCR_seq = paste0(rle(RESULT_def2)$values, collapse = ""), 
            ASSAY_seq = paste0(rle(ASSAY_num)$values, collapse = "")) %>% 
  count(PCR_seq) %>% 
  mutate(`%`= sprintf('%0.2f%%',(n/sum(n)*100)))

result_ASSAY_PCR_def2 <- ct_Imran %>% group_by(ENCNTR_ID) %>% filter(n()>=3) %>% 
  summarise(PCR_seq = paste0(rle(RESULT_def2)$values, collapse = ""), 
            ASSAY_seq = paste0(rle(ASSAY_num)$values, collapse = "")) %>% 
  count(ASSAY_seq,PCR_seq) %>% 
  group_by(ASSAY_seq) %>% 
  mutate(`%`= sprintf('%0.2f%%',(n/sum(n)*100)))

lst_def1 <- list("PCR_All" = result_PCR_def2, "PCR_By_ASSAY" = result_ASSAY_PCR_def2)
write.xlsx(lst_def1, file="result_def2.xlsx", row.names=FALSE)









