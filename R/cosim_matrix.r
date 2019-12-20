#compute the cosinus similarity matrix to how different crisis cluster each others
cosim_matrix=function(table_weights){
  #compute the matrix of cosinus simularity between all the type of crisis 
  #parameters:
  #table_weights: a table of tf-idf with documents in rows and type of crisis in 
  #columns
  
  #vects=names(table_weights)[!names(table_weights) %in% c("file","ISO3_Code","Period","year","variable","Review")] 
  vects=names(table_weights %>% dplyr::select_if(is.numeric))
  res=matrix(NA,length(vects),length(vects),dimnames = list(vects,vects))
  for(j in 1:length(vects)){
    #ind=c(1:(j-1),(j+1):length(vects))
    for(i in 1:length(vects)){#j
      res[i,j]=cosim(table_weights,vects[j],vects[i])
      #print(res)
    }
  }
  
  res[res=="Inf"]=0
  res[res=="NaN"]=0
  res=res %>% round(.,2)
  return(res)
}
