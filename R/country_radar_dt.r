
# Look at average importance of each crisis by country and display the more
# important elements
country_radar_dt = function(table_N_occurence, isoc, top_n = 50, weight_method = "brut_frequency", 
    group = "ISO3_Code") {
    #' Transform the tf-idf database in proper format for drawing the radar chart of categories
    #' Transform the tf-idf database in proper format for drawing the radar chart of categories
    #' @param  table_N_occurence a dataframe with documents in rows and tf-idfs of each categories
    #' in columns
    #' @param isoc the iso3 code of the country to display
    #' @param top_n the first n larger index to display
    #' @param weight_method the type of method for the tf computation one of "brut_frequency",
    #'  "binary_frequency","log_norm_frequency"
    #' @param group the grouping variable of interest
    #' @author Manuel Betin
    #' @return a tible with the value of the tf summarized according to the grouping selected
    #' @export
      
    table = tf_by_group(table_N_occurence, weight_method = weight_method, mygroup = group)
    table=table %>% dplyr::filter(ISO3_Code %in% isoc) %>% dplyr::arrange(-word_weight) %>% 
        top_n(top_n)
    return(table)
}
