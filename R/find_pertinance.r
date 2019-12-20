#generate a search engine for files of crisis

find_pertinance=function(table.N.occurrence,keyword){
  #rand the more pertinent category given the tf-idf
  words=find_associated_keyword(keyword)
  table=tf_idf(table.N.occurrence)
  table=table %>% tidyr::gather(Crisis,value="weight",-c(ISO3_Code,Period,year,type_prog,file))
  results=table %>% dplyr::filter(stringr::str_detect(tolower(Crisis),tolower(keyword)) & weight>0) %>% dplyr::arrange(-weight)
  return(list(keywords=words,results=results))
}
