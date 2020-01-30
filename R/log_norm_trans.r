log_norm_trans = function(table_N_occurence) {
    # log normal transformation of the table
    log_norm_trans = function(x) {
        ifelse(x > 0, 1 + log(x), 0)
    }
    table_log_norm_trans = table_N_occurence %>% dplyr::mutate_if(is.numeric, 
        log_norm_trans)
    return(table_log_norm_trans)
}
