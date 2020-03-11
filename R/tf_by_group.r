
# compute the average importance of each crisis by groups
tf_by_group = function(table_N_occurence, weight_method = "brut_frequency", 
    mygroup = NULL) {
  
  #' Summarize the term frequency matrix by group
  #' Summarize the tf matrix according to the grouping variable selected
  
  #' @param table_N_occurence a tibble of term frequencies from tf(), run_tf(),
  #'  run_tf_update() or run_tf_by_chunk()
  #' @param  weight_method the method for countring "brut_frequency", "binary_frequency" 
  #' @param mygroup the grouping variable to be used for the summary
  #' @author Manuel Betin
  #' @return a dataframe of term frequencies with documents in rows and categories
  #' in columns
  #' @export
  #' 
    if (is.null(mygroup)) {
        avoid_colums = c(names(table_N_occurence %>% dplyr::select_if(is.character)), 
            names(table_N_occurence %>% dplyr::select_if(is.Date)))
        dt_weights_years = table_N_occurence %>% tidyr::gather("Crisis", value = "word_weight", 
            -c(avoid_colums))
        dt_weights_years = dt_weights_years[-1, ]
        dt_weights_years = dt_weights_years %>% dplyr::mutate(word_weight = as.numeric(word_weight))
        return(dt_weights_years)
    } else {
        avoid_colums = c(names(table_N_occurence %>% dplyr::select_if(is.character)), 
            names(table_N_occurence %>% dplyr::select_if(is.Date)))
        
        dt_weights_years = table_N_occurence %>% tidyr::gather("Crisis", value = "word_weight", 
            -c(avoid_colums))
        dt_weights_years = dt_weights_years[-1, ]
        dt_weights_years = dt_weights_years %>% group_by(get(mygroup)) %>% dplyr::mutate(word_weight = as.numeric(word_weight))
        return(dt_weights_years)
    }
}
