radar_structure_fig = function(radar_dt) {
  
  #' Radar chart of the tf 
  #' @description Use the radar_dt() output and draw a radar chart to display the 
  #' term frequencies of the debt structure category. 
  #' @param radar_dt output of country_radar_fig
  #' @return a plotly object 
  #' @author Manuel Betin
  #' @export   
  #' 
  endo_exo_order = c("Concessional_lending", "Short_term_debt", "floating_rate_debt", 
                     "foreign_debt", "Fixed_exchange_rate", "Floating_exchange_rate", "Technical_assistance")
  
  radar_dt = radar_dt %>% dplyr::arrange(match(Crisis, endo_exo_order)) %>% 
    dplyr::filter(Crisis %in% endo_exo_order)
  
  Weights = radar_dt[, "word_weight"]$word_weight * 10000
  
  Weights[Weights == 0] = 1e-07
  Crisis = stringr::str_replace_all(radar_dt[, "Crisis"]$Crisis, "_", 
                                    " ")
  
  alpha_endo = 0.8
  plot <- plotly::plot_ly(type = "scatterpolar", mode = "lines") %>% plotly::add_trace(r = rep(2.5, 
                                                                                               length(Crisis)), theta = Crisis, line = list(color = "lightgrey")) %>% 
    plotly::add_trace(r = Weights, theta = Crisis, name = "Profile of crisis", 
                      line = list(color = "#709Bff"), fill = "toself", fillcolor = "#709Bff", 
                      alpha_endo) %>% plotly::layout(xaxis = 3, yaxis = median(Weights, 
                                                                               na.rm = T)) %>% 
    plotly::layout(xaxis = list(title = "", showgrid = T, zeroline = F, showticklabels = T, 
                                domain = c(0, 3)), yaxis = list(title = radar_dt$ISO3_Code %>% 
                                                                  unique(), showgrid = F, zeroline = F, showticklabels = F), font = list(family = "serif", 
                                                                                                                                         size = 20), legend = list(bgcolor = "transparent", title = "test"), 
                   showlegend = FALSE)
  return(plot)
  
}

radar_shocks_fig = function(radar_dt) {
  
  #' Radar chart of the tf 
  #' @description  Use the radar_dt() output and draw a radar chart to display the 
  #' term frequencies of the selected categories. Indexes are order counterclockwise
  #' from more exogeneous (15:15) to more exogeneous
  #' @param radar_dt output of country_radar_fig
  #' @return a plotly object 
  #' @author Manuel Betin
  #' @export   
  
  endo_exo_order = c("Wars", "Natural_disaster", "Commodity_crisis", "trade_crisis", 
                     "World_outcomes", "Expectations", "Contagion", "Balance_payment_crisis", 
                     "Reduction_reserves", "Currency_crisis", "Banking_crisis", "Financial_crisis", 
                     "Severe_recession", "Soft_recession", "Inflation_crisis", "Political_crisis", 
                     "Social_crisis", "Fiscal_outcomes", "Fiscal_consolidation", "Sovereign_default")
  
  reforms = c("Deregulation", "Reform_agenda", "Trade_reforms", "Financial_reforms", 
              "Labor_market_reforms", "Tax_reforms", "Banking_reforms")
  
  structure = c("Concessional_lending", "Short_term_debt", "floating_rate_debt", 
                "foreign_debt", "Fixed_exchange_rate", "Floating_exchange_rate", "Technical_assistance")
  
  radar_dt = radar_dt %>% dplyr::arrange(match(Crisis, endo_exo_order)) %>% 
    dplyr::filter(Crisis %in% endo_exo_order)
  
  Weights = radar_dt[, "word_weight"]$word_weight * 10000
  
  Weights[Weights == 0] = 1e-07
  Crisis = stringr::str_replace_all(radar_dt[, "Crisis"]$Crisis, "_", 
                                    " ")
  
  i = 6
  j = 6
  s = 5
  alpha_endo = 0.8
  p <- plotly::plot_ly(type = "scatterpolar", mode = "lines") %>% plotly::add_trace(r = rep(2.5, 
                                                                                            length(Crisis)), theta = Crisis, line = list(color = "lightgrey")) %>% 
    plotly::add_trace(r = Weights, theta = Crisis, name = "Profile of crisis", 
                      line = list(color = "#709Bff"), fill = "toself", fillcolor = "#709Bff", 
                      alpha_endo) %>% plotly::layout(xaxis = 3, yaxis = median(Weights, 
                                                                               na.rm = T)) %>% plotly::layout(xaxis = list(title = "", showgrid = T, 
                                                                                                                           zeroline = F, showticklabels = T, domain = c(0, 3)), yaxis = list(title = radar_dt$ISO3_Code %>% 
                                                                                                                                                                                               unique(), showgrid = F, zeroline = F, showticklabels = F), font = list(family = "serif", 
                                                                                                                                                                                                                                                                      size = 20), legend = list(bgcolor = "transparent", title = "test"), 
                                                                                                              showlegend = FALSE)
  p
  
}

radar_reforms_fig = function(radar_dt) {
  #' Radar chart of the tf 
  #' @description Use the radar_dt() output and draw a radar chart to display the 
  #' term frequencies of the reforms categories. 
  #' @param radar_dt output of country_radar_fig
  #' @return a plotly object 
  #' @author Manuel Betin
  #' @export   
  endo_exo_order = c("Deregulation", "Reform_agenda", "Trade_reforms", "Financial_reforms", 
                     "Labor_market_reforms", "Tax_reforms", "Banking_reforms")
  
  radar_dt = radar_dt %>% dplyr::arrange(match(Crisis, endo_exo_order)) %>% 
    dplyr::filter(Crisis %in% endo_exo_order)
  
  Weights = radar_dt[, "word_weight"]$word_weight * 10000
  
  Weights[Weights == 0] = 1e-07
  Crisis = stringr::str_replace_all(radar_dt[, "Crisis"]$Crisis, "_", 
                                    " ")
  
  i = 6
  j = 6
  s = 5
  alpha_endo = 0.8
  plot <- plotly::plot_ly(type = "scatterpolar", mode = "lines") %>% plotly::add_trace(r = rep(2.5, 
                                                                                               length(Crisis)), theta = Crisis, line = list(color = "lightgrey")) %>% 
    plotly::add_trace(r = Weights, theta = Crisis, name = "Profile of crisis", 
                      line = list(color = "#709Bff"), fill = "toself", fillcolor = "#709Bff", 
                      alpha_endo) %>% plotly::layout(xaxis = 3, yaxis = median(Weights, 
                                                                               na.rm = T)) %>% plotly::layout(xaxis = list(title = "", showgrid = T, 
                                                                                                                           zeroline = F, showticklabels = T, domain = c(0, 3)), yaxis = list(title = radar_dt$ISO3_Code %>% 
                                                                                                                                                                                               unique(), showgrid = F, zeroline = F, showticklabels = F), font = list(family = "serif", 
                                                                                                                                                                                                                                                                      size = 20), legend = list(bgcolor = "transparent", title = "test"), 
                                                                                                              showlegend = FALSE)
  return(plot)
  
}

radar_dt = function(tf_data, isoc, top_n = 50, weight_method = "brut_frequency", 
                            group = "ISO3_Code") {
  #' Transform the tf-idf database in proper format for drawing the radar chart of categories
  #' @description Transform the tf-idf database in proper format for drawing the radar chart of categories
  #' @param  tf_data a dataframe with documents in rows and tf-idfs of each categories
  #' in columns
  #' @param isoc the iso3 code of the country to display
  #' @param top_n the first n larger index to display
  #' @param weight_method the type of method for the tf computation one of "brut_frequency",
  #'  "binary_frequency","log_norm_frequency"
  #' @param group the grouping variable of interest
  #' @author Manuel Betin
  #' @return a tible with the value of the tf summarized according to the grouping selected
  #' @export
  
  table = tf_by_group(tf_data, weight_method = weight_method, mygroup = group)
  table=table %>% dplyr::filter(ISO3_Code %in% isoc) %>% dplyr::arrange(-word_weight) %>% 
    top_n(top_n)
  return(table)
}
