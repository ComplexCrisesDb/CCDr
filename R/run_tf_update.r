
run_tf_update = function(path_tf_to_update = "tf_crisis_words.RData", corpus_path = "IMF_letofIntent_1960_2014_clean.RData", 
    type_lexicon = "words", keyword_list = NULL, export_path = "tf_crisis_words.RData", 
    parrallel = T) {
    
    # function that update the tf output of run_tf with the new variables avoid
    # having to rerun all categories
    
    if (is.null(keyword_list)) {
        print("Updating all columns")
        new_tf = run_tf(corpus_path = corpus_path, type_lexicon = type_lexicon, 
            keyword_list = key_words_crisis(), export_path = paste0(root_path, 
                "/3. Data/IMF Letters of Intents/tf_crisis_words.RData"), parrallel = parrallel)
        return(new_tf)
    } else {
        
        cat(crayon::green("updating selected columns"))
        tf_to_update = rio::import(path_tf_to_update)
        dim_tf_to_update = dim(tf_to_update)
        existing_cols = names(tf_to_update)
        
        if (any(existing_cols %in% keyword_list)) {
            tf_to_update = tf_to_update %>% dplyr::select(-keyword_list)
        }
        
        corpus = rio::import(corpus_path)
        
        new_tf = run_tf(corpus_path = corpus_path, type_lexicon = type_lexicon, 
            keyword_list = keyword_list, parrallel = parrallel)
        
        tf_to_update = dplyr::left_join(x = tf_to_update, y = new_tf, by = "file")
        
        print(paste0("Non updated columns:\n
                 ", paste0(existing_cols, 
            collapse = ", ")))
        
        print(paste0("Updated columns:\n
                 ", paste0(keyword_list, 
            collapse = ", ")))
        rio::export(tf_to_update, export_path)
        
        return(tf_to_update)
    }
    
    
}
