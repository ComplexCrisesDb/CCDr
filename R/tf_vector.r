tf_vector = function(corpus, keyword_list, brute_freq = F, parrallel = T) {
    #' vectorize the function tf() to be able to pass a list of names of keywords
    #' keyword_list is a list containing the names of different groups of
    #' keywords that have a vector of words to look into.
    
    #' @param corpus a list of texts from pdf_text() with different names
    #' for each element
    #' @param keyword_list the names of the items in key_word_crisis to
    #'  include in the computation
    #'  @param brute_freq T/F if T it will just count the occurence, otherwise
    #'  it will compute the term frequency
    #'  @param parrallele T/F if T it will use mclapply from the parrallel package
    #'  @author Manuel Betin
    #'  @return a tibble with the term frequencies for the selected categories
    #'  @export
    
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
    
    # If more than one index, reduce the merge.
    
    if(names(keyword_list > 1)){
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
    }
    
    # At this stage dataframe with file name and all different indexes.
    # Include additional step to net out index computed on confusing keywords (e.g. referring to other countries):
    
    # List of "confusing" categories on which the index could be computed:
    
    list_net_keywords <- str_extract(names(key_words_crisis()), ".+_confusing")[complete.cases(str_extract(names(key_words_crisis()), ".+_confusing"))]
     
    # Check if we computed some of them:
    
    if (any(list_net_keywords %in% names(dt))){
 
     dt = split.default(dt, str_remove(names(dt), "_.+"))  # split into list according to the first word of column name, removing the rest.
     
     dt = dt %>%    
     purrr::map(~ if (any(names(.x) != "file")) {                               
       if (any(str_detect(names(.x), "confusing"))) {                               # if column name with "confusing" within same category 
            colnames(.x)[grepl('confusing',colnames(.x))] <- "confusing"            # subtract from other indexes.
            .x %>% 
              mutate_all(funs(. - confusing)) %>% 
              select(-confusing)
          } else {
            .x
          }
       }
       else {
         .x
       }) 
     
     
      dt %>%                                          # Bind net indexes with file list.
        purrr::reduce(cbind) %>% 
        select(file,everything())
     
    } else {
       dt
    }
  }
