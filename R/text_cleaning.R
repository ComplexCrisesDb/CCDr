remove_stop <- function(word) {
  #' remove stop words
  #' @description remove stop words
  #' @param word that as to be checked
  #' @author Manuel Betin
  stop <- ifelse(word %in% stop_words$word, "", word)
  return(stop)
}

clean_text <- function(file) {
  #' clean character strings
  #' @description clean character string by removing special characters
  #' and irrelevant strings
  #' @param file a string of characters to clean
  #' @return a clean character string
  #' @author Manuel Betin

  # file=stringr::str_replace_all(file,'[^a-zA-Z\\s]', ' ')
  file <- stringr::str_replace_all(file, "[\\s]+", " ")
  file <- gsub("\r", "", file)
  file <- gsub("\"", "", file)
  file <- gsub("\n", "", file) %>%
    iconv("utf-8", "ascii", sub = "") %>%
    str_replace_all("-\\s", "") %>%
    str_replace_all("~\\s", "")
  file <- file[stringr::str_count(file) > 50]
  return(file)
}
