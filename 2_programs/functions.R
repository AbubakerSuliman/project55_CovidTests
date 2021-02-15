cal_timediff <- function(df_r) {
  #browser()
  if(all(df_r[['flag_seq']] %in% c('1','10'))){
    mutate(df_r, diff_max=NA, diff_min=NA)
  } else {
    vec <- df_r[["RESULT_num"]]
    vec_t <- df_r[["DATE_COLLECTED"]]
    
    strs <- which(c(0, diff(vec))==-1)
    ends <- which(c(0, diff(vec))==1)
    
    strs <- strs[seq_len(length(ends))]
    
    dfftimes <- map2_dbl(strs, ends, ~difftime(vec_t[.y], vec_t[.x], , units = 'days'))
    mutate(df_r, diff_max = max(dfftimes, na.rm = TRUE), 
           diff_min = min(dfftimes, na.rm = TRUE))
    
  }
}

##################################################################
# PCR PATTERNS ###################################################
##################################################################
rl <- function(x){
  #browser()
  rl <- rle(x)
  
  if(first(rl$values)==1 & last(rl$values)==0){
    p1 <- switch(as.character(length(rl$values)==2),
                 "TRUE" = "1;0|1,0",
                 "FALSE" = "1;0|1,x,0"
    )
  } else if(first(rl$values)==1 & last(rl$values)==1){
    p1 <- switch(as.character(n_distinct(rl$values)==1),
                 "TRUE" = "1;1|1,1",
                 "FALSE" = "1;1|1,x,1"
    )
  } else {
    p1 <- 'Not Defined|Not Defined'
  }
  
  p2 <- paste0("{",rl$values,":",ifelse(rl$lengths==1,"1","\u22652"),"}", collapse = " ")
  
  return(paste0(p1,"|",p2))
  
}