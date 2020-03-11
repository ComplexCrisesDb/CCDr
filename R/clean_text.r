clean_text = function(file) {
    #' clean character strings
    #' clean character string by removing special characters 
    #' and irrelevant strings
    #' @param file a string of characters to clean
    #' @return a clean character string 
    #' @author Manuel Betin
    #' @export
   
    # file=stringr::str_replace_all(file,'[^a-zA-Z\\s]', ' ')
    file = stringr::str_replace_all(file, "[\\s]+", " ")
    file = gsub("\r", "", file)
    file = gsub("\"", "", file)
    file = gsub("\n", "", file)
    file = file[stringr::str_count(file) > 50]
    file = gsub("<U+23AF>", "", file)
    return(file)
}
