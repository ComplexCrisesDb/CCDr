print.corpusTM <- function(x, ...) {
  #' Printing CorpusTM
  #' S3 method for class "corpus.TM"
  #' @param x object of class "corpus.TM"
  #' @param ... optional arguments to ```print```
  #' @author Manuel Betin, Umberto Collodel
  #' @noRd
  cat("\n")
  cat("Number of documents: ", length(x), "\n")
  cat("First document: ", names(x)[[1]], "\n")
  cat("Last document: ", tail(names(x), n = 1), "\n")
  cat("\n")
}
