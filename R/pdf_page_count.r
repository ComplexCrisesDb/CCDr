pdf_page_count=function(files){
  #count the number of pages in the pdf
  #parameters:
  # files: a list of character strings
  
  error_no_metadata=try({files[[1]]$info},silent = T)
  if("try-error" %in% class(error_no_metadata)){
    cat(crayon::red("No metadata of document available, please run aggregate_corpus() setting the argument only_files=F \n"))
    #return(NULL)
  }else{
    table_count=lapply(files,function(x){
      #n.words=count_words(x$file)# %>% arrange(n) #%>% dplyr::summarize(word=paste(paste(word,'(',n,')',sep=""),collapse=", "))
      n.pages=x$info$pages
      n.pages
      #list(n.words=n.words,n.pages=n.pages)
    })
    
    names(table_count)=names(files)
    table_count=do.call(rbind,table_count)
    table_count=data.frame(table_count)
    table_count$File=names(files)
    table_count %>% dplyr::select(File,N.pages=table_count)
  }
}
