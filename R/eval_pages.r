eval_pages=function(files,targetword,brute_freq=F,parrallel=T){
  #provide list of files and return summary of counts of occurence of the target world 
  #parameters:
  #file:a character string correspond to the text to analysis
  # targetword: a vector of characters corresponding to word to search and count for 
  # in the text
  # brute_freq: T if you only want the frequency count of the word, if F the output is the 
  # it provides the term frequency for the set of targetwords provided
  # parrallel: if T the function use the mclapply function to use parrallele computing
  # from the package 'parrallele"
  
  if(parrallel){
    metric=parallel::mclapply(files,function(x){
      file=find_pages(x,targetword)
      if(brute_freq){
        file$Tot.occurence
      }else{file$Tot.occurence/file$N.chars}
      
    }, mc.cores = parallel::detectCores()-1, mc.allow.recursive = TRUE)
    N.Occurence=do.call(rbind,metric)
    if(length(targetword)==1){
      colnames(N.Occurence)=targetword
    }else colnames(N.Occurence)="Occurence"
  }else{
    metric=lapply(files,function(x){
      file=find_pages(x,targetword)
      if(brute_freq){
        file$Tot.occurence
      }else{file$Tot.occurence/file$N.chars}
      
    })
    N.Occurence=do.call(rbind,metric)
    if(length(targetword)==1){
      colnames(N.Occurence)=targetword
    }else colnames(N.Occurence)="Occurence"
  }
  # N.Occurence=data.frame(N.Occurence) %>% dplyr::mutate(ISO3_Code=substr(rownames(a),4,6),
  #                            year=substr(rownames(a),str_locate(rownames(a), "_2")+1,str_locate(rownames(a), "_2")+4),
  #                            path_file=substr(rownames(a),16,80),
  #                            Occurence=as.numeric(as.character(Occurence)))
  return(N.Occurence)
}
