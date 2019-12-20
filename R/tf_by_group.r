
#compute the average importance of each crisis by groups
tf_by_group=function(table_N_occurence,weight_method="brut_frequency",mygroup=NULL){
  if(is.null(mygroup)){
    #dt_weights_years=tf_idf(table_N_occurence,weight_method) %>% ungroup() #dplyr::summarize_if(is.numeric,mean)
    avoid_colums=c(names(table_N_occurence %>% dplyr::select_if(is.character)),names(table_N_occurence %>% dplyr::select_if(is.Date)))
    dt_weights_years=table_N_occurence %>% tidyr::gather("Crisis",value="word_weight",-c(avoid_colums))
    dt_weights_years=dt_weights_years[-1,]
    dt_weights_years=dt_weights_years %>% dplyr::mutate(word_weight=as.numeric(word_weight))
    dt_weights_years
  }else{  
    avoid_colums=c(names(table_N_occurence %>% dplyr::select_if(is.character)),names(table_N_occurence %>% dplyr::select_if(is.Date)))
    
    dt_weights_years=table_N_occurence %>% tidyr::gather("Crisis",value="word_weight",-c(avoid_colums))
    dt_weights_years=dt_weights_years[-1,]
    dt_weights_years=dt_weights_years %>% group_by(get(mygroup)) %>% dplyr::mutate(word_weight=as.numeric(word_weight))
    
  }
}
