# formula for the cosinus similiarity that provides the distance between two
# vectors here two crisis
cosim = function(tf_data, vec1, vec2) {
  #' Compute cosinus similarity of two vectors
  #'@description Measure the distance between two vectors using cosinus similarity
  #' formula
  #' @param tf_data a tibble with documents in rows and number 
  #' of occurence of each type of crisi in columns
  #' @param vec1 the name of the column 1 to analyse
  #' @param vec2 the name of the column 2 to analyse 
  #' @return A number providing the similarity between the two vectors
  #' between the two vectors
  #' @author Manuel Betin
  #' @export
  
  res = tf_data %>% 
    dplyr::select(vec1, vec2) %>%
    dplyr::mutate(scalar_prod = get(vec1) * get(vec2), norm_1 = (get(vec1))^2, norm_2 = (get(vec2))^2) %>%
    dplyr::summarize(scalar_prod = sum(scalar_prod), 
                     norm_1 = sum(norm_1), norm_2 = sum(norm_2)) %>%
    dplyr::mutate(cosinus_similarity = scalar_prod/(sqrt(norm_1) * 
                                                      sqrt(norm_2))) %>%
    dplyr::select(cosinus_similarity)
  
  return(res$cosinus_similarity)
}

# compute the cosinus similarity matrix to how different crisis cluster each
# others
cosim_matrix = function(tf_data) {
  #' Compute all pairwise cosinus similarity for a dataframe of indexes
  #' 
  #'@description  compute the matrix of cosinus simularity between all the type of crisis
  #' @param tf_data a table of tf-idf with documents in rows and
  #' type of crisis in columns
  #' @author Manuel Betin
  #' @return a symetrical matrix with the pairwise cosinus similarity
  #' @export
  
  vects = names(tf_data %>% dplyr::select_if(is.numeric))
  res = matrix(NA, length(vects), length(vects), dimnames = list(vects, vects))
  for (j in 1:length(vects)) {
    for (i in 1:length(vects)) {
      res[i, j] = cosim(tf_data, vects[j], vects[i])
    }
  }
  
  res[res == "Inf"] = 0
  res[res == "NaN"] = 0
  res = res %>% round(., 2)
  return(res)
}

# plot results of matric cosinus
cosim_fig = function(cosim_matrix, var) {
  #' barpot of the cosinus similarity
  #' @description  barpot of the cosinus similarity
  #' @param cosim_matrix the cosinus similarity matrix from cosim_matrix()
  #' @param var the target variable 
  #' @return a ggplot barplot
  #' @author Manuel Betin
  #' @export
  #' 
  dt = cosim_matrix %>% dplyr::select(myvar = var)
  dt$xaxis = rownames(dt)
  dt = dt %>% dplyr::mutate(xaxis = str_replace(xaxis, "_", " "))
  
  plot=ggplot2::ggplot(data = dt) + ggplot2::geom_bar(stat = "identity", aes(x = reorder(xaxis, 
                                                                                         -myvar), y = myvar)) + ggplot2::theme_bw() + ggplot2::ylab("Cosinus similarity") + 
    ggplot2::xlab("") + ggplot2::theme(legend.position = "bottom", axis.text.x = ggplot2::element_text(angle = 90, 
                                                                                                       hjust = 1), axis.text = ggplot2::element_text(size = 8))
  return(plot)
}
