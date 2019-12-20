unsupervized_corpus=function(corpus,length_ngram=2,min_occurence=10){
  mycorpus=lapply(1:length(corpus),function(x){
    text=corpus[[x]]
    tictoc::tic()
    cleaned_ngram=clean_ngram(text,length_ngram = length_ngram)  
    cleaned_ngram=cleaned_ngram %>% dplyr::mutate(file=names(corpus)[x])
    cleaned_ngram=cleaned_ngram %>% dplyr::select(word,file,n,everything())
    tictoc::toc()
    cleaned_ngram
  })
  mycorpus=do.call(rbind,mycorpus)
  mycorpus=mycorpus %>% dplyr::filter(!grepl("\\d",word))
  assoc_words=list()
  assoc_words[["crisis"]]=assoc_words(mycorpus,"crisis",min_occurence = min_occurence)
  assoc_words[["banks"]]=assoc_words(mycorpus,"banks",min_occurence = min_occurence)
  assoc_words[["debt"]]=assoc_words(mycorpus,"debt",min_occurence = min_occurence)
  assoc_words[["reform"]]=assoc_words(mycorpus,"reform",min_occurence = min_occurence)
  assoc_words[["budget"]]=assoc_words(mycorpus,"budget",min_occurence =min_occurence )
  return(list(corpus=mycorpus,associated_words=associated_words))
}
