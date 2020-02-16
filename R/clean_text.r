clean_text = function(file) {
    # first step cleaning of the file by removing preselected character
    # file=stringr::str_replace_all(file,'[^a-zA-Z\\s]', ' ')
    file = stringr::str_replace_all(file, "[\\s]+", " ")
    file = gsub("\r", "", file)
    file = gsub("\"", "", file)
    file = gsub("\n", "", file)
    file = file[stringr::str_count(file) > 50]
    file = gsub("<U+23AF>", "", file)
    return(file)
}
