tf_vector = function(corpus, keyword_list, brute_freq = F, parrallel = T) {
    # vectorize the function tf() to be able to pass a list of names of keywords
    # keyword_list is a list containing the names of different groups of
    # keywords that have a vector of words to look into.
    
    # parameteres: corpus: a list of texts from pdf_text() with different names
    # for each element keyword_list: the names of the items in
    # keyword_list_crisis to include in the computation
    
    # outptut: a table with each row corresponding to a document and each colum
    # providing the number of occurence for the given item.
    
    # my_keywords=key_words_crisis()
    progress = dplyr::progress_estimated(length(keyword_list))
    list_table_keyword_occurence = lapply(1:length(keyword_list), function(x) {
        cat(crayon::bgBlack("\n"))
        cat(crayon::green(paste0("(", x, "/", length(keyword_list), ") running: ", 
            names(keyword_list)[x], "\n")))
        tictoc::tic(names(keyword_list)[x])
        if (!"character" %in% class(keyword_list[[x]])) {
            warning("please provide a valid vector of characters")
            dt = NULL
            tictoc::toc()
            dt
        } else {
            res = try({
                dt = tf(corpus, keyword_list[[x]], brute_freq = brute_freq, 
                  parrallel = parrallel)
                dt = dt %>% dplyr::rename(`:=`(!!paste0(names(keyword_list)[x]), 
                  var))
                dt
            })
            if ("try-error" %in% class(dt)) {
                cat(crayon::red(paste0("\n Error: term frequency as not been computed", 
                  "\n")))
                res = NULL
            }
            tictoc::toc()
            progress$pause(0.01)$tick()$print()
            cat(crayon::green(paste0("\n Finished running: ", names(keyword_list)[x], 
                "\n")))
            res
        }
    })
    names(list_table_keyword_occurence) = names(keyword_list)
    
    dt = list_table_keyword_occurence[[1]]
    for (i in 2:length(list_table_keyword_occurence)) {
        res = try({
            dt = dt %>% dplyr::left_join(list_table_keyword_occurence[[i]], 
                by = c("file"))  #'ISO3_Code','Period','type_prog','year',  
            dt
        })
        if ("try-error" %in% class(res)) {
            dt = dt
        } else {
            dt = res
            dt = dt %>% dplyr::select(file, everything()) %>% dplyr::distinct()  #ISO3_Code,Period,year,type_prog,
        }
    }
    
    return(dt)
}
