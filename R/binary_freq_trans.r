# functions to transform brut frequency into alternative metrics
binary_freq_trans = function(table_N_occurence) {
    #' binary transformation of tfidf
    
    #' transform table from Number of occurence to binary variables
    
    #' @param table_N_occurence a dataframe with numerical columns corresponding
    #' to the tf idf of each category
    #' @author Manuel Betin
    #' @return a dataframe with binary frequencies 0 if the tf_idf is zero and 1
    #' if it is non zero
    #' @export
    
    binary_trans = function(x) {
        ifelse(x > 0, 1, 0)
    }
    table_brut_frequency = table_N_occurence %>% dplyr::mutate_if(is.numeric, 
        binary_trans)
    return(table_brut_frequency)
}
