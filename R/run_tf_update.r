#'Updates the tf-indexes without repeating full extraction
#'
#' Combine probability of shocks, intensity and complexity of relations to construct
#' .a mesure of severity of crisis
#'
#' @param path_tf_to_update Path to old tf dataframe.
#' @param corpus_path Path to corpus from which perform the extraction.
#' @param type_lexicon Character: "words" or "category"
#' @param keyword_list Character vector: names of character vectors to use for extraction.
#' @param export_path Path to export the file.
#' @param parrallel Logical. If TRUE, parallel computation for each category.
#' @param store_old Logical. If TRUE, store old extractions in a directory.
#' @param store_old_path Path to move old extractions in.
#'
#' @return A dataframe with file name, old indexes not specified in keyword_list and updated indexes.
#'
#' @author Manuel Bétin, Umberto Collodel
#'
#' @examples
#'
#'
#' @export


run_tf_update = function(path_tf_to_update = "tf_crisis_words.RData", corpus_path = "IMF_letofIntent_1960_2014_clean.RData", 
    type_lexicon = "words", keyword_list = NULL, export_path = "tf_crisis_words.RData", 
    parrallel = T, store_old = F, store_old_path = NULL) {
    
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
          # Remove only columns in both update list and old dataframe:
            existing_keyword_list <- intersect(existing_cols, keyword_list)
            tf_to_update = tf_to_update %>% dplyr::select(- existing_keyword_list)
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
        
        if(store_old == T){
          if(dir.exists(store_old_path) != T){
            dir.create(store_old_path)
          }
          # Move old files:
          file.move(path_tf_to_update, store_old_path, overwrite = TRUE)
        }
          
        rio::export(tf_to_update, export_path)
        
        return(tf_to_update)
    }
    
    
}
