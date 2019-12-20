
clean_ngram=function(text,length_ngram=4){
  #generate tibble with ngrams of specified length in rows and dummies 
  #for  a set of typologies, the function clean the text by removing 
  #unnecessary characters and removing common stop words.
  
  #parameters:
  # text: a character string containing the text to analyse
  # length_ngram the number of word to consider has the unit of observation
  
  #output:
  # a tibble with ngrams in rows, number of occurence and dummies for the typologies 
  text= text %>% clean_text() %>% tibble() 
  text=text %>% unnest_tokens(ngram, text, token = "ngrams", n = length_ngram)
  
  #remove stop words
  
  list_nwords=paste("word",1:length_ngram,sep="")
  ngrams_sep=text %>% 
    dplyr::select(ngram) %>% 
    tidyr::separate(ngram, list_nwords, sep = " ")
  ngrams_sep=ngrams_sep %>% dplyr::mutate_at(vars(matches("word")),remove_stop) %>%
    tidyr::unite("word",list_nwords,sep=" ") %>%
    dplyr::select(word) %>%
    dplyr::mutate(word=stringr::str_trim(word),
                  word=stringr::str_replace(word,"  "," ")) %>% dplyr::filter(word!="")
  
  
  #remove figures and numeric variables 
  ngrams_sep=ngrams_sep %>% dplyr::filter(!stringr::str_detect(word,"\\d") | !stringr::str_detect(word,"perce") )
  
  ngrams_sep=ngrams_sep %>%
    dplyr::count(word,sort=T)
  
  #filter type of words
  ngrams_sep=ngrams_sep %>% dplyr::mutate(crisis=ifelse(str_detect(word,"crisis") | 
                                                          stringr::str_detect(word,"distress") |
                                                          stringr::str_detect(word,"downturn"),1,0),
                                          law=ifelse(stringr::str_detect(word,"law") | 
                                                       stringr::str_detect(word,"legal") |
                                                       stringr::str_detect(word,"regulation")|
                                                       stringr::str_detect(word,"supervision"),1,0),
                                          finance=ifelse(stringr::str_detect(word,"finance"),1,0),
                                          supervision=ifelse(stringr::str_detect(word,"supervis"),1,0),
                                          solvency=ifelse(stringr::str_detect(word,"solven"),1,0),
                                          liquidity=ifelse(stringr::str_detect(word,"liquid"),1,0),
                                          budget=ifelse(stringr::str_detect(word,"budget"),1,0),
                                          guarantee=ifelse(stringr::str_detect(word,"guarantee"),1,0),
                                          nationalization=ifelse(stringr::str_detect(word,"nationalization"),1,0),
                                          overdue=ifelse(stringr::str_detect(word,"overdue"),1,0),
                                          accomodative=ifelse(stringr::str_detect(word,"accomodative"),1,0),
                                          administrative=ifelse(stringr::str_detect(word,"administrative"),1,0),
                                          economic=ifelse(stringr::str_detect(word,"economic"),1,0),
                                          money=ifelse(stringr::str_detect(word,"money"),1,0),
                                          arrears=ifelse(stringr::str_detect(word,"arrears"),1,0),
                                          exchange=ifelse(stringr::str_detect(word,"exchange"),1,0),
                                          depreciation=ifelse(stringr::str_detect(word,"depreciation"),1,0),
                                          banks=ifelse(stringr::str_detect(word,"bank"),1,0),
                                          capital=ifelse(stringr::str_detect(word,"capital"),1,0),
                                          asset=ifelse(stringr::str_detect(word,"asset"),1,0),
                                          reform=ifelse(stringr::str_detect(word,"reform"),1,0),
                                          debt=ifelse(stringr::str_detect(word,"debt"),1,0),
                                          loans=ifelse(stringr::str_detect(word,"loan")|
                                                         stringr::str_detect(word,"portfolio")|
                                                         stringr::str_detect(word,"lend"),1,0)) 
  
  
  #filter according to 
  ngrams_sep=ngrams_sep %>% dplyr::filter(crisis==1 | law==1 |finance==1 |
                                            supervision==1 |solvency==1 |liquidity==1 |
                                            budget==1 |guarantee==1 |nationalization==1 |
                                            overdue==1 |accomodative==1 |administrative==1 |
                                            economic==1 |money==1 |arrears==1 |exchange==1 |
                                            depreciation==1 |banks==1 |capital==1 |
                                            asset==1 |reform==1 |debt==1 |loans==1)
  
  return(ngrams_sep)
}
