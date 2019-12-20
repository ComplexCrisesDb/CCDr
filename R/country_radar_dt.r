
#Look at average importance of each crisis by country and display the more important elements
country_radar_dt=function(table_N_occurence,isoc,top_n=50,weight_method="brut_frequency",group="ISO3_Code"){
  table=tf_by_group(table_N_occurence,weight_method = weight_method,mygroup=group)
  table %>% dplyr::filter(ISO3_Code %in% isoc) %>% dplyr::arrange(-word_weight) %>% top_n(top_n)
} 
