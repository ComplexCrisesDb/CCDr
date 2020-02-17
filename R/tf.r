tf = function(corpus, keywords, brute_freq = F, parrallel = T) {
    # for each document in the corpus the function creates a table counting the
    # occurence of the words in the vector of keywords and provide a table with
    # the number of occurence, name of the file that is the name of each
    # document in the corpus (from which we can extract: the country code,
    # period, type of program and year)
    
    # parameters: corpus: a list of texts from pdf_text() with different names
    # for each element keywords: a vector of strings containing the targeted
    # words to look for
    
    if (is.vector(keywords) & !is.null(keywords)) {
        table = lapply(corpus, function(x) {
          
          # Exclude minutes meetings, press releases and working papers erroneously scraped.
          # Check the first page of each document for bad keyword.
          
          valid_document <- tibble(first_page = tolower(x[[1]])) %>% 
            filter(!str_detect(first_page, paste(key_words_crisis()[["Problematic_documents"]], collapse = "|"))) %>% 
            nrow()
          
          # If is a valid document, continue with tf-calculation. Else, return NA.
          
          if (valid_document == 1){
                output = try(sum(eval_pages(x, keywords, brute_freq = brute_freq, 
                parrallel = parrallel)[, 1]))
            if ("try-error" %in% class(output)) {
                cat(crayon::red(paste0("Warning: error when mining ", keywords)))
                output = 0
            }
            output
          }
        else { 
          NA
        }
        })
        names(table) = names(corpus)
        table = do.call(rbind, table)
        table = dplyr::tibble(table)
        table$file = names(corpus)
        names(table) = c("var", "file")
        return(table)
    } else {
        cat(crayon::red("please provide a vector of strings as argument for keywords"))
    }
}



