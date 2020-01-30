idf = function(table_N_occurence, group = NULL) {
    # compute the inverse document frequency as the logarithm of the inverse of
    # the proportion. it allows to give reduce weight of words with high
    # frequency in the corpus
    
    N.doc.corpus = dim(table_N_occurence)[1]
    idf_trans = function(x) {
        if (x == 0) {
            x
        } else {
            log(N.doc.corpus/sum(x))
        }
    }
    
    table_N_binary = binary_freq_trans(table_N_occurence)
    
    if (!is.null(group)) {
        inverse_doc_freq = table_N_binary %>% dplyr::group_by(get(group)) %>% 
            dplyr::summarize_if(is.numeric, sum) %>% dplyr::mutate_if(is.numeric, 
            idf_trans) %>% gather("Crisis", value = "idf", -"get(group)")
        colnames(inverse_doc_freq)[1] = group
    } else {
        inverse_doc_freq = table_N_binary %>% dplyr::summarize_if(is.numeric, 
            sum) %>% dplyr::mutate_if(is.numeric, idf_trans) %>% gather("Crisis", 
            value = "idf")
    }
    return(inverse_doc_freq)
}
