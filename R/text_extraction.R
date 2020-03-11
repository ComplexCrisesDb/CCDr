
pdf_from_url = function(urls, export_path, overwrite = T) {
  #' download pdf documents 
  #' download from a a dataframe containing the url of the files
  #' the pdf of interest
  #' @param urls a dataframe with a row by document and at least the following columns
  #' title, pdf, and name_file. title contains the title of the document, pdf the url 
  #' where to download the files and name_file the name that will be given to the pdf
  #' @param export_path path where to store the downloaded pdfs
  #' @param overwrite T the function re download the file and overwrite the existing 
  #' @return creates a folder and store the pdf
  #' @author Manuel Betin
  #' @export
  
  urls=data.frame(urls)
  ref_colnames = c("title","pdf", "name_file")
  
  if (!dir.exists(export_path)) {
    # create new folder if not existing
    dir.create(export_path, recursive = T)
  }
  log = list()
  if (any(names(urls) %in% ref_colnames)) {
    # make sure the files has the proper format with at least the name of the
    # file and a url link to pdf
    count = 0
    progress = dplyr::progress_estimated(dim(urls)[1])
    lapply(1:dim(urls)[1], function(i) {
      count <<- count + 1
      filename = urls[i, "name_file"]
      tictoc::tic(paste0(urls[i, "name_file"], " : ", count, "/", dim(urls)[1]))  #progress bar to follow the evolution of the downloads
      # Download every documents and overwrite existing
      if (overwrite == T) {
        file <- try(download.file(urls[i, "pdf"], destfile = paste0(export_path, 
                                                                    "/", urls[i, "name_file"], ".pdf")), silent = T)
        if ("try-error" %in% class(file)) {
          # handle errors in the url or errors in the download of the file
          cat(crayon::red(paste(urls[i, "name_file"], ": Error in path file: ", 
                                urls[i, "pdf"], sep = "")))
          log[urls[i, "name_file"]] = "Error on download"
          file = NA
        } else {
          log[urls[i, "name_file"]] = "Download completed"
          cat(crayon::green(paste(urls[i, "name_file"], ": succesfully downloaded", 
                                  sep = "")))
        }
      } else {
        # Download only missing documents
        if (!file.exists(paste0(export_path, "/", urls[i, "name_file"], 
                                ".pdf"))) {
          file <- try(download.file(urls[i, "pdf"], destfile = paste0(export_path, 
                                                                      "/", urls[i, "name_file"], ".pdf")), silent = T)
          if ("try-error" %in% class(file)) {
            # handle errors in the url or errors in the download of the file
            cat(crayon::red(paste(urls[i, "name_file"], ": Error in path file: ", 
                                  urls[i, "pdf"], sep = "")))
            log[urls[i, "name_file"]] = "Error on download"
            file = NA
          } else {
            log[urls[i, "name_file"]] = "Download completed"
            cat(crayon::green(paste(urls[i, "name_file"], ": succesfully downloaded \n", 
                                    sep = "")))
          }
        } else {
          log[urls[i, "name_file"]] = "Old version kept"
          cat(crayon::blue(paste(urls[i, "name_file"], ": already downloaded, keep existing \n", 
                                 sep = "")))
        }
      }
      progress$pause(0.01)$tick()$print()
      tictoc::toc()
      file
    })
    # export a log on the details of the extraction
    rio::export(log, paste0("log_downloads_", Sys.time(), ".RData"))
    cat(crayon::green(paste0("urls succesfully downloaded in '", export_path, 
                             "\n'")))
    
    # print('urls succesfully downloaded')
  } else {
    cat(crayon::red("Please provide a valid data.frame of with at least two columns: name_file (name of your file) and pdf (the url link) \n"))
    
  }
  
}

pdf_page_count = function(files) {
  #' count the number of pages in the pdf
  #' count the number of pages in the pdf
  #' @param files a list of character strings
  #' @return the number of pages in the document
  #' @author Manuel Betin
  #' @export
  
  error_no_metadata = try({
    files[[1]]$info
  }, silent = T)
  if ("try-error" %in% class(error_no_metadata)) {
    cat(crayon::red("No metadata of document available, please run aggregate_corpus()
                    setting the argument only_files=F \n"))
    # return(NULL)
  } else {
    table_count = lapply(files, function(x) {
      # n.words=count_words(x$file)# %>% arrange(n) #%>%
      # dplyr::summarize(word=paste(paste(word,'(',n,')',sep=''),collapse=', '))
      n.pages = x$info$pages
      n.pages
      # list(n.words=n.words,n.pages=n.pages)
    })
    
    names(table_count) = names(files)
    table_count = do.call(rbind, table_count)
    table_count = data.frame(table_count)
    table_count$File = names(files)
    table_count=table_count %>% dplyr::select(File, N.pages = table_count)
    return(table_count)
  }
}

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

eval_pages = function(files, targetword, brute_freq = F, parrallel = T) {
  #'Look for the presence of the targetword into a character string
  #' Provide list of files and return summary of counts of occurence of the
  #' target world 
  #' @param files a character string correspond to the text to
  #' analysis
  #' @param targetword a vector of characters corresponding to word to
  #' search and count for in the text 
  #' @param brute_freq T if you only want the frequency count of the word,
  #'if F the output is the it provides the term frequency for the set of 
  #'targetwords provided
  #' @param parrallel if T the function use the mclapply function to use
  #'  parrallele computing from the package "parrallele"
  #'  @author Manuel Betin
  #'  @return  return the number of occurence of the targetword for each file 
  #'  @export
  
  if (parrallel) {
    metric = parallel::mclapply(files, function(x) {
      file = get_pages(x, targetword)
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
      file = get_pages(x, targetword)
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

get_pages = function(file, targetword) {
  #' Find the page where words a located
  #' Provide files either pdf of html and return the paragraphs matching the
  #' targetted word 
  #' @param file a character string correspond to the text
  #' to analysis
  #' @param targetword a vector of characters corresponding to word to
  #' search and count for in the text
  #' @author Manuel Betin
  #' @return a list of strings of characters where words have been found.
  #' @export
  #' 
  if (!is.null(file)) {
    page_locations = list()
    target_pages = list()
    i = 0
    Tot.occurence = 0
    file = clean_text(file)
    n.chars = sum(nchar(file))  #total number of characters in the file after cleaning
    page_location = sapply(1:length(file), function(x) {
      page = file[[x]]
      page = tibble(page) %>% tidytext::unnest_tokens(word, page, token = "sentences") %>%
        dplyr::mutate(word = tolower(word))
      n.occurence = (page %>% dplyr::filter(grepl(paste(tolower(targetword), 
                                                        collapse = "|"), word)) %>% dplyr::summarize(count = n()))$count
      condition = n.occurence > 0
      if (any(condition)) {
        i <<- i + 1
        page_locations[[i]] <<- paste0("Found in page ", x, " :", n.occurence, 
                                       " times")
        target_pages[[i]] <<- file[[x]]
        Tot.occurence <<- Tot.occurence + n.occurence
        return("yes")
      } else {
        return("no")
      }
      
    })
    
    return(list(target = targetword, N.chars = n.chars, N.Occurence = page_locations, 
                Tot.occurence = Tot.occurence, pages = target_pages))
  } else return(list(target = targetword, N.chars = 0, N.Occurence = 0, Tot.occurence = 0, 
                     pages = 0, error_message = "File is null no mining provided"))
}

get_sentences <- function(corpus, keyword_list){
  #'From a corpus (collection documents), returns the sentences where keyword detected
  #'and respective keyword
  #'
  #'Function that simplifies checking validity indexes and potential problems in keywords
  #'
  #' @param corpus list with collection documents
  #' @param keyword_list character vector with categories from key_words_crisis function
  #'
  #' @return Tibble with two columns: sentences containing keyword and respective keyword. 
  #'
  #' @author Manuel Betin, Umberto Collodel
  #'
  #' @examples
  #'
  #'
  #' @export
  
  corpus %>% 
    map(~ tibble(document = .x)) %>% 
    map(~ .x %>% tidytext::unnest_tokens(sentence, document, token = "sentences")) %>% 
    map(~ .x %>% filter(str_detect(sentence, paste(lexicon()[[keyword_list]], collapse = "|")))) %>% 
    discard(~ nrow(.x) == 0) %>% 
    map(~ .x %>% mutate(keyword_detected = str_extract(sentence, paste(lexicon()[[keyword_list]], collapse = "|"))))
}

log_norm_trans = function(tf_data) {
  #' log normal transformation of the table
  #' log normal transformation of the table
  #' @param tf_data a dataframe of term frequencies
  #' @return a tibble of tf with log norm transformation
  #' @author Manuel Betin
  #' @export
  log_norm_trans = function(x) {
    ifelse(x > 0, 1 + log(x), 0)
  }
  table_log_norm_trans = tf_data %>% dplyr::mutate_if(is.numeric, 
                                                      log_norm_trans)
  return(table_log_norm_trans)
}

binary_freq_trans = function(tf_data) {
  #' binary transformation of tfidf
  
  #' transform table from Number of occurence to binary variables
  
  #' @param tf_data a dataframe with numerical columns corresponding
  #' to the tf idf of each category
  #' @author Manuel Betin
  #' @return a dataframe with binary frequencies 0 if the tf_idf is zero and 1
  #' if it is non zero
  #' @export
  
  binary_trans = function(x) {
    ifelse(x > 0, 1, 0)
  }
  table_brut_frequency = tf_data %>% dplyr::mutate_if(is.numeric, 
                                                      binary_trans)
  return(table_brut_frequency)
}
