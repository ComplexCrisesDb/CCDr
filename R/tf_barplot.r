tf_barplot=function(tf,
                    vars_type=c("economic_shock","non_economic_shock","debt_outcomes","debt_structure","characteristics_program","adustment_program"),
                    vars_nature=NULL){
  #take as input the term frequency matrix and reshape it to plot the values for all numeric columns and group them in 
  #two dimensions: the type of variable and the nature of the shock (exogeneous or endogenous)
  
  res=list()
  avg_tf=tf %>% mutate_if(is.numeric,funs(ifelse(.==0,NA,.))) %>%
    ungroup() %>%
    summarize_if(is.numeric,funs(mean(.,na.rm=T)*100)) %>%
    gather(key="variable",value="tf_idf")
  
  avg_tf=avg_tf %>% left_join(typology_categories(),by=c("variable"))
  
  res[["tf_table_avg"]]=avg_tf
  
  if(!is.null(vars_nature)){
    avg_tf=avg_tf %>% filter(type %in% vars_type & nature_shock %in% vars_nature)
  }else{avg_tf=avg_tf %>% filter(type %in% vars_type)}
  
  #avg_tf=avg_tf %>% filter(type %in% vars_filter)
  avg_tf=avg_tf%>% mutate(variable=str_replace_all(variable,"_"," ")) 
  prop_avg_tf=avg_tf %>% mutate(tf_idf=tf_idf/sum(tf_idf)) 
  
  if(!is.null(vars_nature)){
    res[["tf_fig_avg"]]=ggplot(avg_tf)+
      geom_bar(stat="identity",aes(x=reorder(variable,tf_idf),y=tf_idf,fill=type))+
      geom_text(aes(x=reorder(variable,tf_idf),y=tf_idf*0.8,label=substr(nature_shock,1,3)),color="white")+
      scale_fill_manual(values=c("darkred","blue","darkgreen","lightgrey","orange","brown"))+
      theme_bw()+
      #lims(y=c(0,1))+
      labs(#title="Average intensity of the events",
        x=NULL,y="Term frequency (%)")+
      theme(legend.title=element_blank(),legend.position="bottom",axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=8))
  }else{
    res[["tf_fig_avg"]]=ggplot(avg_tf)+
      geom_bar(stat="identity",aes(x=reorder(variable,tf_idf),y=tf_idf,fill=nature_shock))+
      scale_fill_manual(values=c("darkred","blue","darkgreen","lightgrey","orange","brown"))+
      theme_bw()+
      #lims(y=c(0,1))+
      labs(#title="Average intensity of the events",
        x=NULL,y="Term frequency (%)")+
      theme(legend.title=element_blank(),legend.position="bottom",axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=8))
  }
  if(!is.null(vars_nature)){
    res[["tf_fig_avg_prop"]]=ggplot(prop_avg_tf)+
      geom_bar(stat="identity",aes(x=reorder(variable,tf_idf),y=tf_idf,fill=type))+
      geom_text(aes(x=reorder(variable,tf_idf),y=tf_idf*0.8,label=substr(nature_shock,1,3)),color="white")+
      scale_fill_manual(values=c("darkred","blue","darkgreen","lightgrey","orange","brown"))+
      theme_bw()+
      #lims(y=c(0,1))+
      labs(#title="Average intensity of the events",
        x=NULL,
        y=" % of total shock related words")+
      theme(legend.title=element_blank(),legend.position="bottom",axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=10))
  }else{
    res[["tf_fig_avg_prop"]]=ggplot(prop_avg_tf)+
      geom_bar(stat="identity",aes(x=reorder(variable,tf_idf),y=tf_idf,fill=nature_shock))+
      #geom_text(aes(x=reorder(Crisis,tf_idf),y=tf_idf,label=nature),rotate=90)+
      scale_fill_manual(values=c("darkred","blue","darkgreen","lightgrey","orange","brown"))+
      theme_bw()+
      #lims(y=c(0,1))+
      labs(#title="Average intensity of the events",
        x=NULL,
        y=" % of total shock related words")+
      theme(legend.title=element_blank(),legend.position="bottom",axis.text.x=element_text(angle=90,hjust=1),axis.text=element_text(size=10))
  }
  res
}
