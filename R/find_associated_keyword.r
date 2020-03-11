
find_associated_keyword = function(keyword) {
    #' Provide the keywords associate to the name of the category
    #' usefull know what are the words behind each category
    #' @param keyword the name of the category of which you want to 
    #' know the lexicon
    #' @author Manuel Betin
    #' @return a vector of words 
    #' @export
    detect = names(key_words_crisis())[stringr::str_detect(tolower(names(key_words_crisis())), 
        tolower(keyword))]
    
    return(key_words_crisis()[detect])
    
}
