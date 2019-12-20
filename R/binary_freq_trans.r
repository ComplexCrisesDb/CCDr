#functions to transform brut frequency into alternative metrics
binary_freq_trans=function(table_N_occurence){
  #transform table from Number of occurence to binary variables
  binary_trans=function(x){ifelse(x>0,1,0)}
  table_brut_frequency=table_N_occurence %>% dplyr::mutate_if(is.numeric,binary_trans)
  return(table_brut_frequency)
}
