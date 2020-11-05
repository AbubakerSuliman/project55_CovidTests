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
