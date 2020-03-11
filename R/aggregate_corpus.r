# function to aggregate corpus
aggregate_corpus = function(path_files, only_files = F) {
    #' Aggregate pdf files into list of characters
    #'
    #' function that takes the path of the directory and load all
    #' the pdfs of the directory into a list in order to further perform the text
    #' mining
    
    #' @param path_files the path of the directory with the files
    #' @param only_files T/F whether to include in the list only the content or also
    #' the metadata of the pdf file
    #' @author Manuel Betin
    #' @return A list containing the content of each document 
    #' @export
    #'
    docs = list.files(path_files, pattern = ".pdf",ignore.case = T)
    docs = stringr::str_remove(docs, ".PDF")
    docs = stringr::str_remove(docs, ".pdf")
    count = 0
    start = 1
    x = 1
    corpus = lapply(start:length(docs), function(x) {
        count <<- count + 1
        tictoc::tic(paste0(count, "/", length(docs), " ", docs[x]))
        path = paste0(path_files, "/", docs[x], ".pdf")
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
