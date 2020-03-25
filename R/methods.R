print.corpusTM <- function(x,...){
  #' Printing CorpusTM
  #' S3 method for class "corpus.TM"
  #' @param x object of class "corpus.TM"
  #' @parm optional arguments to ```print```
  #' @author Manuel Betin, Umberto Collodel
  #' @export
  cat("\n")
  cat("Number of documents: ",length(corpus),"\n") 
  cat("First document: ",names(corpus)[[1]],"\n")
  cat("Last document: ",tail(names(corpus), n = 1),"\n")
  cat("\n")
}
