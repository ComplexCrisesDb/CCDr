#formula for the cosinus similiarity that provides the distance between two vectors here
#two crisis
cosim=function(table_N_occurence,vec1,vec2){
  #formula for the cosinus similiarity that provides the distance/similarity between two vectors here
  #two crisis
  #pararmeters:
  # table_N_occurence: a tibble with documents in rows and number of occurence of each type of crisi
  # in columns
  # vec1: the name of the column 1 to analyse
  # vec2: the name of the column 2 to analyse
  #output:
  # a number providing the similarity between the two vectors
  res=table_N_occurence %>% dplyr::select(vec1,vec2) %>%
    dplyr::mutate(scalar_prod=get(vec1)*get(vec2),
                  norm_1=(get(vec1))^2,
                  norm_2=(get(vec2))^2) %>%
    dplyr::summarize(scalar_prod=sum(scalar_prod),
                     norm_1=sum(norm_1),
                     norm_2=sum(norm_2)) %>%
    dplyr::mutate(cosinus_similarity=scalar_prod/(sqrt(norm_1)*sqrt(norm_2))) %>%
    dplyr::select(cosinus_similarity)
  
  return(res$cosinus_similarity)
}
