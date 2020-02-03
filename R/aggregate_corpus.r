# function to aggregate corpus
aggregate_corpus = function(path_files, only_files = F) {
    # Description: function that takes the path of the directory and load all
    # the pdfs of the directory into a list in order to further perform the text
    # mining
    
    # parameters: path_files: the path of the directory with the files
    
    # output: a list containing the content of each document
  
    docs = list.files(path_files, pattern = ".pdf")
    docs = stringr::str_remove(docs, ".PDF")
    docs = stringr::str_remove(docs, ".pdf")
    count = 0
    start = 1
    x = 1
    corpus = lapply(start:length(docs), function(x) {
        count <<- count + 1
        tictoc::tic(paste0(count, "/", length(docs), " ", docs[x]))
        path = paste0(path_files, "/", docs[x], ".PDF")
        file <- try({
            pdfinfo = pdf_info(path)
            pdf_text(path) %>% strsplit(split = "\n")
        }, silent = T)
        if ("try-error" %in% class(file)) {
            warning(paste(docs[[x]], ": Error in path file", sep = ""))
            pdfinfo=NA
            file = NA
        } else {
            file = clean_text(file)
        }
        print(path)
        tictoc::toc()
        if (only_files == T) {
            file
        } else {
            list(info = pdfinfo, file = file)
        }
    })
    names(corpus) = docs
    return(corpus)
}
