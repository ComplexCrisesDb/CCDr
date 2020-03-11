eval_pages = function(files, targetword, brute_freq = F, parrallel = T) {
    #'Look for the presence of the targetword into a character string
    #' Provide list of files and return summary of counts of occurence of the
    #' target world 
    #' @param file a character string correspond to the text to
    #' analysis
    #' @param targetword a vector of characters corresponding to word to
    #' search and count for in the text 
    #' @param brute_freq: T if you only want the frequency count of the word,
    #'if F the output is the it provides the term frequency for the set of 
    #'targetwords provided
    #' @param parrallel if T the function use the mclapply function to use
    #'  parrallele computing from the package "parrallele"
    #'  @author Manuel Betin
    #'  @return  return the number of occurence of the targetword for each file 
    #'  @export
    
    if (parrallel) {
        metric = parallel::mclapply(files, function(x) {
            file = find_pages(x, targetword)
            if (brute_freq) {
                file$Tot.occurence
            } else {
                file$Tot.occurence/file$N.chars
            }
            
        }, mc.cores = parallel::detectCores() - 1, mc.allow.recursive = TRUE)
        N.Occurence = do.call(rbind, metric)
        if (length(targetword) == 1) {
            colnames(N.Occurence) = targetword
        } else colnames(N.Occurence) = "Occurence"
    } else {
        metric = lapply(files, function(x) {
            file = find_pages(x, targetword)
            if (brute_freq) {
                file$Tot.occurence
            } else {
                file$Tot.occurence/file$N.chars
            }
            
        })
        N.Occurence = do.call(rbind, metric)
        if (length(targetword) == 1) {
            colnames(N.Occurence) = targetword
        } else colnames(N.Occurence) = "Occurence"
    }
    
    return(N.Occurence)
}
