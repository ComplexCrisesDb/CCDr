radar_reforms_fig=function(country_radar_dt){
  
  # endo_exo_order=c("Wars","Natural_disaster","Commodity_crisis","trade_crisis","World_outcomes",
  #                  "Expectations","Contagion","Balance_payment_crisis","Reduction_reserves",
  #                  "Currency_crisis","Banking_crisis","Financial_crisis","Severe_recession","Soft_recession",
  #                  "Inflation_crisis","Political_crisis","Social_crisis","Fiscal_outcomes","Fiscal_consolidation","Sovereign_default")
  # 
  endo_exo_order=c("Deregulation","Reform_agenda","Trade_reforms","Financial_reforms","Labor_market_reforms","Tax_reforms","Banking_reforms")
  # 
  # structure=c("Concessional_lending","Short_term_debt","floating_rate_debt","foreign_debt","Fixed_exchange_rate","Floating_exchange_rate","Technical_assistance")
  # 
  country_radar_dt=country_radar_dt %>%
    dplyr::arrange(match(Crisis,endo_exo_order)) %>% dplyr::filter(Crisis %in% endo_exo_order)
  
  Weights=country_radar_dt[,"word_weight"]$word_weight*10000
  
  Weights[Weights==0]=0.0000001
  Crisis=stringr::str_replace_all(country_radar_dt[,"Crisis"]$Crisis,"_"," ")
  
  i=6
  j=6 
  s=5
  alpha_endo=0.8
  p <- plotly::plot_ly(type = 'scatterpolar',
                       mode="lines") %>%
    plotly::add_trace(
      r = rep(2.5,length(Crisis)), #max(Weights)
      theta = Crisis,
      line=list(color="lightgrey")) %>%
    # add_trace(
    #   r = c(rep(0,i),rep(max(Weights,na.rm=T),j),rep(0,length(Crisis)-j-i)),
    #   theta = Crisis,
    #   name="Exogenous",
    #   line=list(color=toRGB("orange",alpha_endo)),
    #   fill = 'toself',fillcolor = toRGB("orange",alpha_endo)) %>%
    # add_trace(
    #   r = c(rep(max(Weights,na.rm=T),i),rep(0,length(Crisis)-i)),
    #   theta = Crisis,
    #   name="Pure Exogenous",
    #   line=list(color=toRGB("green",alpha_endo)),
  #   fill = 'toself',fillcolor = toRGB("green",alpha_endo)) %>%
  # add_trace(
  #   r = c(rep(0,length(Crisis)-s),rep(max(Weights,na.rm=T),s)),
  #   theta = Crisis,
  #   name="Endogenous",
  #   line=list(color=toRGB("red",alpha_endo)),
  #   fill = 'toself',fillcolor = toRGB("red",alpha_endo)) %>%
  plotly::add_trace(
    r = Weights,
    theta = Crisis,
    name="Profile of crisis",
    line=list(color="#709Bff"),
    fill = 'toself',fillcolor = '#709Bff',alpha_endo) %>%
    plotly::layout(xaxis = 3, yaxis = median(Weights,na.rm=T)) %>%
    
    plotly::layout(
      xaxis = list(title = "", showgrid = T, zeroline = F, showticklabels = T,
                   domain = c(0, 3)
      ),
      yaxis = list(title = country_radar_dt$ISO3_Code %>% unique(), showgrid = F, zeroline = F, showticklabels = F#,
                   #domain = c(0, 0.92)
      ),
      font = list(family = "serif", size = 20),
      legend = list(bgcolor = "transparent",title="test"),
      showlegend = FALSE
    )
  p
  
}
