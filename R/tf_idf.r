# function to compute the weights of each word by doing the product of the
# index of frequency and the inverse document frequency
tf_idf = function(table_N_occurence, weight_method = "brut_frequency") {
    # function to compute the weights of each word by doing the product of the
    # index of frequency and the inverse document frequency
    dt_inv_doc_freq = try(idf(table_N_occurence))
    if ("try_error" %in% dt_inv_doc_freq) {
        cat(crayon::red(paste0("Warning: error when using function dt_inv_doc_freq")))
        return(NULL)
    }
    select_cols = names(table_N_occurence %>% dplyr::select_if(is.numeric))
    
    if (weight_method == "brut_frequency") {
        dt_words_weight = table_N_occurence
        for (var in select_cols) {
            dt_words_weight[, var] = dt_words_weight[, var] * (dt_inv_doc_freq %>% 
                dplyr::filter(Crisis == var) %>% dplyr::select(idf))[[1]]
        }
        return(dt_words_weight)
    } else if (weight_method == "binary_frequency") {
        dt_words_weight = binary_freq_trans(table_N_occurence)
        for (var in select_cols) {
            dt_words_weight[, var] = dt_words_weight[, var] * (dt_inv_doc_freq %>% 
                dplyr::filter(Crisis == var) %>% dplyr::select(idf))[[1]]
        }
        return(dt_words_weight)
    } else if (weight_method == "log_norm_frequency") {
        dt_words_weight = log_norm_trans(table_N_occurence)
        for (var in select_cols) {
            dt_words_weight[, var] = dt_words_weight[, var] * (dt_inv_doc_freq %>% 
                dplyr::filter(Crisis == var) %>% dplyr::select(idf))[[1]]
        }
        return(dt_words_weight)
    } else warning("please choose a proper method: brut_frequency,binary_frequency,log_norm_frequency")
}
