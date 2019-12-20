
remove_stop=function(word){
  #remove stop words
  ifelse(word %in% stop_words$word,"",word)
}
