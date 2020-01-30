
remove_stop = function(word) {
    # remove stop words
   stop= ifelse(word %in% stop_words$word, "", word)
   return(stop)
}
