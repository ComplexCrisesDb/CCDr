ccdr.transcripts.compile=function(myvar,ctries,path_corpus,path_export){
  #' Get transcripts for a specific variable
  #' @description use the CCDB dictionary to extract and label each sentence of 
  #' the country reports.
  #' @param myvar the name of the category in the dictionary that will be extracted
  #' @param ctries a vector of countries of which extract the transcripts
  #' @param path_corpus the path of the directory where the corpus for each country
  #' are located 
  #' @param path_export the path of the directory where to save the specific transcripts
  #' @author Manuel Betin
  #' @export
  
  mytranscripts=lapply(ctries,function(x){
    #path to the country specific corpus
    mycorpus_path=paste0(path_corpus,x)
    print(paste0(x,"\n"))
    #import corpus
    tryCatch({
      mycorpus=rio::import(mycorpus_path)
      # Collect the transcripts where references to specific crisis appear
      transcripts_all=ccdr.transcripts.collect(mycorpus,myvar)
      transcripts_all
    },
    error=function(e){
      warning(e)
      NULL
    })
  })
  mytranscriptsdt=do.call(rbind,mytranscripts)
  
  # Export transcripts
  rio::export(mytranscriptsdt,
              paste0(path_export,"ccdb_",myvar,"_transcripts.csv"))
  
  return(mytranscriptsdt)
}
