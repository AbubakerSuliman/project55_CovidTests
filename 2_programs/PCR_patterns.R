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
df_base <- read_csv("1_data/df_ep1.csv")

#Read PCR Tests
fls_nms <- list.files(paste0(path_data,'/final'), pattern = ".*_\\d.csv") %>% setNames(.,nm = gsub('\\D+','',.))

df0 <- map_df(fls_nms,
              ~read_csv(paste0(path_data,'/final/',.x),
                        col_types = cols(DATE_COLLECTED = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
                                         RESULT_PERFORMED_DT = col_datetime(format = "%m/%d/%Y %H:%M:%S"), 
                                         RESULT_VERIFY_DATE = col_datetime(format = "%m/%d/%Y %H:%M:%S"))) ,
              .id = "part")

df0 <- filter(df0, PERSON_ID %in% df_base$PERSON_ID)

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
  filter(n()>=3) 

df3 <- df2 %>% group_by(PERSON_ID) %>% 
  mutate(flag_seq = rl(x=RESULT_num)) %>% slice(1) %>% tidyr::separate(flag_seq, into=c('M1',"M2","M3"), sep="\\|")

#All
df_pa_all <- df3 %>% group_by(M1,M2,M3) %>% count(name = 'n_M3') %>% 
  mutate(M4=ifelse(!grepl('x',M2),M3,paste(first(str_split(M3,"\\s")[[1]]),"x",last(str_split(M3,"\\s")[[1]])))) %>%
  select(M1, M2, M4, M3, n_M3)


#Summary
df_pa_sum <- df3 %>% mutate(M4=ifelse(!grepl('x',M2),M3,paste(first(str_split(M3,"\\s")[[1]]),"x",last(str_split(M3,"\\s")[[1]])))) %>% 
  group_by(M1,M2,M4) %>% count(name = 'n_M4') %>% 
  group_by(M1) %>% mutate(n_M1=sum(n_M4)) %>% 
  group_by(M2) %>% mutate(n_M2=sum(n_M4)) %>% 
  mutate_at(vars(n_M4,n_M1,n_M2), ~sprintf("%s (%.0f%%)",format(., big.mark = ","),(./13956)*100)) %>% 
  select(M1, M2, M4, n_M1, n_M2, n_M4)


#Write data to Excel
#create a named list of your dataframes. The list names will be the worksheet names.
xl_lst <- list('Summary' = df_pa_sum, 'All' = df_pa_all)
write.xlsx(xl_lst, file="1_data/PCR_patterns.xlsx", row.names=FALSE)

