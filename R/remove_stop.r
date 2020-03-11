
remove_stop = function(word) {
    #' remove stop words
    #' remove stop words
    #' @param word that as to be checked
    #' @author Manuel Betin
    #' @export
   stop= ifelse(word %in% stop_words$word, "", word)
   return(stop)
}
