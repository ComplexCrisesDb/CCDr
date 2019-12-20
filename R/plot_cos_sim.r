
#plot results of matric cosinus
plot_cos_sim=function(cos_sim,var){
  dt= cos_sim %>% dplyr::select(myvar=var) 
  dt$xaxis=rownames(dt)
  dt=dt%>% dplyr::mutate(xaxis=str_replace(xaxis,"_"," "))
  
  ggplot2::ggplot(data=dt)+
    ggplot2::geom_bar(stat="identity",aes(x=reorder(xaxis,-myvar),y=myvar))+
    ggplot2::theme_bw()+
    ggplot2::ylab("Cosinus similarity")+
    ggplot2::xlab("")+
    ggplot2::theme(legend.position="bottom",axis.text.x=ggplot2::element_text(angle=90,hjust=1),axis.text=ggplot2::element_text(size=8))
  
}
