
find_associated_keyword = function(keyword) {
    # find the keywords associate to the name of the category provide usefull to
    # know what are the words behind each category
    detect = names(key_words_crisis())[stringr::str_detect(tolower(names(key_words_crisis())), 
        tolower(keyword))]
    
    return(key_words_crisis()[detect])
    
}
