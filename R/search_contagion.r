
search_contagion=function(file){
  tictoc::tic()
  ctry_table=key_words_countries()
  
  ctries=c(ctry_table$ctries,ctry_table$adjectival,ctry_table$Currency,ctry_table$Symbol,key_words_crisis()$Crisis_contagion)
  
  text= clean_text(file)
  text_df <- dplyr::tibble(text = text) %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::anti_join(stop_words,by=c("word"))
  tictoc::toc()
  text_df %>% dplyr::filter(word %in% tolower(ctries)) %>% count(word) %>% dplyr::arrange(word) %>% dplyr::filter(n>1)
}


