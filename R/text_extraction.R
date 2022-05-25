ccdr.corpus <- function(path_files, ENGINE = pdf_text, only_files = F) {
  #' Aggregate pdf files into list of characters
  #'
  #' function that takes the path of the directory and load all
  #' the pdfs of the directory into a list in order to further perform the text
  #' mining
  
  #' @param path_files the path of the directory with the files
  #' Function to read pdf into environment, either pdf_text or pdf_ocr_text, depending on whether image or not.
  #' @param ENGINE the engine for pdf extraction (pdf_text or pdf_ocr_text from pdftools)
  #' @param only_files T/F whether to include in the list only the content or also
  #' the metadata of the pdf file
  #' @author Manuel Betin
  #' @return A list containing the content of each document
  #' @export
  #'
  docs <- list.files(path_files, pattern = ".pdf", ignore.case = T)
  docs <- stringr::str_remove(docs, ".PDF")
  docs <- stringr::str_remove(docs, ".pdf")
  count <- 0
  start <- 1
  x <- 1
  x=1
  corpus <- lapply(start:length(docs), function(x) {
    count <<- count + 1
    tictoc::tic(paste0(count, "/", length(docs), " ", docs[x]))
    path <- paste0(path_files, "/", docs[x], ".pdf")
    file=tryCatch({
      pdfinfo <- pdftools::pdf_info(path)
      ENGINE(path)
    },
    error=function(e){
      warning(paste(docs[[x]], ": Error in path file or in the pdf extraction engine \n", sep = ""))
      warning(e)
      pdfinfo <- NA
      file <- NA
    },
    finally=function(){
      file <- clean_text(file)  
    },silent=T)
    
    tictoc::toc()
    # if (only_files == T) {
    file
    #} else {
    #  list(info = pdfinfo, file = file)
    #}
  })
  names(corpus) <- docs
  attr(corpus, "class") <- c("corpusTM", "list")
  return(corpus)
}

ccdr.pages <- function(files, targetword, brute_freq = F, parrallel = T) {
  #' Look for the presence of the targetword into a character string
  #' Provide list of files and return summary of counts of occurence of the
  #' target world
  #' @param files a character string correspond to the text to
  #' analysis
  #' @param targetword a vector of characters corresponding to word to
  #' search and count for in the text
  #' @param brute_freq T if you only want the frequency count of the word,
  #' if F the output is the it provides the term frequency for the set of
  #' targetwords provided
  #' @param parrallel if T the function use the mclapply function to use
  #'  parrallele computing from the package "parrallele"
  #' @author Manuel Betin
  #' @return  return the number of occurence of the targetword for each file
  #' @export
  
  if (parrallel) {
    metric <- parallel::mclapply(files, function(x) {
      file <- get_pages(x, targetword)
      if (brute_freq) {
        file$Tot.occurence
      } else {
        file$Tot.occurence / file$N.chars
      }
    }, mc.cores = parallel::detectCores() - 1, mc.allow.recursive = TRUE)
    N.Occurence <- do.call(rbind, metric)
    if (length(targetword) == 1) {
      colnames(N.Occurence) <- targetword
    } else {
      colnames(N.Occurence) <- "Occurence"
    }
  } else {
    metric <- lapply(files, function(x) {
      file <- get_pages(x, targetword)
      if (brute_freq) {
        file$Tot.occurence
      } else {
        file$Tot.occurence / file$N.chars
      }
    })
    N.Occurence <- do.call(rbind, metric)
    if (length(targetword) == 1) {
      colnames(N.Occurence) <- targetword
    } else {
      colnames(N.Occurence) <- "Occurence"
    }
  }
  
  return(N.Occurence)
}

ccdr.sentences <- function(corpus, keyword_list) {
  #' From a corpus (collection documents), returns the sentences where keyword detected
  #' and respective keyword
  #'
  #' Function that simplifies checking validity indexes and potential problems in keywords
  #'
  #' @param corpus list with collection documents
  #' @param keyword_list character vector with categories from key_words_crisis function
  #'
  #' @return Tibble with two columns: sentences containing keyword and respective keyword.
  #'
  #' @author Manuel Betin, Umberto Collodel
  #'
  #'
  #'
  #' @export
  
  corpus %>%
    map(~ tibble(document = .x)) %>%
    map(~ .x %>% tidytext::unnest_tokens(sentence, document, token = "sentences")) %>%
    map(~ .x %>% filter(str_detect(sentence, paste(ccdr.lexicon()[[keyword_list]], collapse = "|")))) %>%
    discard(~ nrow(.x) == 0) %>%
    map(~ .x %>% mutate(keyword_detected = str_extract(sentence, paste(ccdr.lexicon()[[keyword_list]], collapse = "|"))))
}

scrap.ccdr.files <- function(urls, export_path, overwrite = T) {
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

  urls <- data.frame(urls)
  ref_colnames <- c("title", "pdf", "name_file")

  if (!dir.exists(export_path)) {
    # create new folder if not existing
    dir.create(export_path, recursive = T)
  }
  log <- list()
  if (any(names(urls) %in% ref_colnames)) {
    # make sure the files has the proper format with at least the name of the
    # file and a url link to pdf
    count <- 0
    progress <- dplyr::progress_estimated(dim(urls)[1])
    lapply(1:dim(urls)[1], function(i) {
      count <<- count + 1
      filename <- urls[i, "name_file"]
      tictoc::tic(paste0(urls[i, "name_file"], " : ", count, "/", dim(urls)[1])) # progress bar to follow the evolution of the downloads
      # Download every documents and overwrite existing
      if (overwrite == T) {
        file <- try(download.file(urls[i, "pdf"], destfile = paste0(
          export_path,
          "/", urls[i, "name_file"], ".pdf"
        )), silent = T)
        if ("try-error" %in% class(file)) {
          # handle errors in the url or errors in the download of the file
          cat(crayon::red(paste(urls[i, "name_file"], ": Error in path file: ",
            urls[i, "pdf"],
            sep = ""
          )))
          log[urls[i, "name_file"]] <- "Error on download"
          file <- NA
        } else {
          log[urls[i, "name_file"]] <- "Download completed"
          cat(crayon::green(paste(urls[i, "name_file"], ": succesfully downloaded",
            sep = ""
          )))
        }
      } else {
        # Download only missing documents
        if (!file.exists(paste0(
          export_path, "/", urls[i, "name_file"],
          ".pdf"
        ))) {
          file <- try(download.file(urls[i, "pdf"], destfile = paste0(
            export_path,
            "/", urls[i, "name_file"], ".pdf"
          )), silent = T)
          if ("try-error" %in% class(file)) {
            # handle errors in the url or errors in the download of the file
            cat(crayon::red(paste(urls[i, "name_file"], ": Error in path file: ",
              urls[i, "pdf"],
              sep = ""
            )))
            log[urls[i, "name_file"]] <- "Error on download"
            file <- NA
          } else {
            log[urls[i, "name_file"]] <- "Download completed"
            cat(crayon::green(paste(urls[i, "name_file"], ": succesfully downloaded \n",
              sep = ""
            )))
          }
        } else {
          log[urls[i, "name_file"]] <- "Old version kept"
          cat(crayon::blue(paste(urls[i, "name_file"], ": already downloaded, keep existing \n",
            sep = ""
          )))
        }
      }
      progress$pause(0.01)$tick()$print()
      tictoc::toc()
      file
    })
    # export a log on the details of the extraction
    rio::export(log, paste0("log_downloads_", Sys.time(), ".RData"))
    cat(crayon::green(paste0(
      "urls succesfully downloaded in '", export_path,
      "\n'"
    )))

    # print('urls succesfully downloaded')
  } else {
    cat(crayon::red("Please provide a valid data.frame of with at least two columns: name_file (name of your file) and pdf (the url link) \n"))
  }
}

pdf_page_count <- function(files) {
  #' count the number of pages in the pdf
  #' count the number of pages in the pdf
  #' @param files a list of character strings
  #' @return the number of pages in the document
  #' @author Manuel Betin
  #' @noRd
  error_no_metadata <- try(
    {
      files[[1]]$info
    },
    silent = T
  )
  if ("try-error" %in% class(error_no_metadata)) {
    cat(crayon::red("No metadata of document available, please run ccdr.corpus()
                    setting the argument only_files=F \n"))
    # return(NULL)
  } else {
    table_count <- lapply(files, function(x) {
      # n.words=count_words(x$file)# %>% arrange(n) #%>%
      # dplyr::summarize(word=paste(paste(word,'(',n,')',sep=''),collapse=', '))
      n.pages <- x$info$pages
      n.pages
      # list(n.words=n.words,n.pages=n.pages)
    })

    names(table_count) <- names(files)
    table_count <- do.call(rbind, table_count)
    table_count <- data.frame(table_count)
    table_count$File <- names(files)
    table_count <- table_count %>% dplyr::select(File, N.pages = table_count)
    return(table_count)
  }
}

get_pages <- function(file, targetword) {
  #' Find the page where words a located
  #' Provide files either pdf of html and return the paragraphs matching the
  #' targetted word
  #' @param file a character string correspond to the text
  #' to analysis
  #' @param targetword a vector of characters corresponding to word to
  #' search and count for in the text
  #' @author Manuel Betin
  #' @return a list of strings of characters where words have been found.
  #' @noRd
  #'
  if (!is.null(file)) {
    page_locations <- list()
    target_pages <- list()
    i <- 0
    Tot.occurence <- 0
    file <- clean_text(file)
    n.chars <- sum(nchar(file)) # total number of characters in the file after cleaning
    page_location <- sapply(1:length(file), function(x) {
      page <- file[[x]]
      page <- tibble(page) %>%
        tidytext::unnest_tokens(word, page, token = "sentences") %>%
        dplyr::mutate(word = tolower(word))
      n.occurence <- (page %>% dplyr::filter(grepl(paste(tolower(targetword),
        collapse = "|"
      ), word)) %>% dplyr::summarize(count = n()))$count
      condition <- n.occurence > 0
      if (any(condition)) {
        i <<- i + 1
        page_locations[[i]] <<- paste0(
          "Found in page ", x, " :", n.occurence,
          " times"
        )
        target_pages[[i]] <<- file[[x]]
        Tot.occurence <<- Tot.occurence + n.occurence
        return("yes")
      } else {
        return("no")
      }
    })

    return(list(
      target = targetword, N.chars = n.chars, N.Occurence = page_locations,
      Tot.occurence = Tot.occurence, pages = target_pages
    ))
  } else {
    return(list(
      target = targetword, N.chars = 0, N.Occurence = 0, Tot.occurence = 0,
      pages = 0, error_message = "File is null no mining provided"
    ))
  }
}

log_norm_trans <- function(tf_data) {
  #' log normal transformation of the table
  #' log normal transformation of the table
  #' @param tf_data a dataframe of term frequencies
  #' @return a tibble of tf with log norm transformation
  #' @author Manuel Betin
  #' @noRd
  log_norm_trans <- function(x) {
    ifelse(x > 0, 1 + log(x), 0)
  }
  table_log_norm_trans <- tf_data %>% dplyr::mutate_if(
    is.numeric,
    log_norm_trans
  )
  return(table_log_norm_trans)
}

binary_freq_trans <- function(tf_data) {
  #' binary transformation of tfidf

  #' transform table from Number of occurence to binary variables

  #' @param tf_data a dataframe with numerical columns corresponding
  #' to the tf idf of each category
  #' @author Manuel Betin
  #' @return a dataframe with binary frequencies 0 if the tf_idf is zero and 1
  #' if it is non zero
  #' @noRd

  binary_trans <- function(x) {
    ifelse(x > 0, 1, 0)
  }
  table_brut_frequency <- tf_data %>% dplyr::mutate_if(
    is.numeric,
    binary_trans
  )
  return(table_brut_frequency)
}

check_extract <- function(path_urls = "../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_Requests_Reviews_articleIV.RData",
                          path_tf_idf = "../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData",
                          path_final_tf_idf = "../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData") {


  #' check the validity of each steps in the scripts to see what documents are lost
  #' in each step of the process from the download, to the
  #' text mining and finally to the merging with the rest of the variables

  #' @param path_urls the path to locate the dataframe of urls (output of 1. Consolidate_urls.R)
  #' @param path_tf_idf: path to locate the tf_idf matrix (output of 2. Run_text_mining.R)
  #' @param path_final_tf_idf: path to locate the final database containing the tf of the text mining and the
  #' rest of the variables merge after (output of 3. Clean_database.R)
  #' @author Manuel Betin
  #' @return a list with a table given the proportion of the files that have not been properly computed by country
  #' the files that have an error and have not been downloaded and the files that have not been downloaded and
  #' merged
  #' @noRd

  metadata <- rio::import(path_urls) %>%
    mutate(name_file = paste0(ID, "_", period, "_", type_doc_programs))
  vars_metadata <- names(metadata)
  size_metadata <- dim(metadata)
  name_file_metadata <- metadata$name_file %>% unique()

  # tf_matrix after running the text mining
  tf <- rio::import(path_tf_idf)
  vars_tf <- names(tf)
  size_tf <- dim(tf)
  name_file_tf <- tf$file %>% unique()

  # tf_idf matrix combining all the available data
  final_data <- rio::import(path_final_tf_idf)
  vars_final_data <- names(final_data)
  size_final_data <- dim(final_data)
  name_file_final_data <- final_data$file %>% unique()

  lost_files_during_textmining <- setdiff(name_file_metadata, name_file_tf)
  missing_urls <- metadata %>% filter(name_file %in% lost_files_during_textmining)

  error_in_url <- missing_urls %>% filter(ID %in% tf$ISO3_Code)
  # proportion of documents properly downloaded
  prop_error_in_url <- metadata %>%
    mutate(missing_urls = ifelse(name_file %in% lost_files_during_textmining, 1, 0)) %>%
    group_by(ID) %>%
    mutate(N_tot_docs = n()) %>%
    ungroup() %>%
    group_by(ID, missing_urls) %>%
    summarize(Prop_missing_urls = 1 - n() / first(N_tot_docs)) %>%
    filter(missing_urls == 0) %>%
    ungroup() %>%
    dplyr::select(-missing_urls)

  return(list(
    proportion_error_in_url = prop_error_in_url,
    files_with_error_in_url = error_in_url,
    files_not_downloaded = missing_urls
  ))
}

check_gaps_pdfs <- function(general_path = "/Volumes/Elements/IMF documents/", only_large = TRUE) {

  #' check if there are gaps greater than one year in the documents

  #' @param general_path path with one subdirectory per individual and for each of them a files subdirectory with pdfs
  #' @param only_large Logical. Consider only gaps greater than two years. Default is TRUE.
  #' @author Manuel Betin, Umberto Collodel
  #' @return a nested list: for every individual the number of gaps and a dataframe with the details.
  #' @noRd

  # Names folders with pdfs by individual:

  names_individual <- list.files(general_path) %>%
    str_extract("[A-Z]{3}") %>%
    na.omit()

  # Extract from pdf folder individual units the year of the file and calculate difference with its lag:
  # (if difference greater than 1, means there is at least a year gap)

  list_year_files <- names_individual %>%
    purrr::map(~ paste0("/Volumes/Elements/IMF documents/", .x, "/files")) %>%
    purrr::map(~ list.files(.x)) %>%
    purrr::map(~ str_extract(.x, "\\d{4}")) %>%
    purrr::map(~ data.frame(year_files = as.numeric(.x))) %>%
    purrr::map(~ .x %>% mutate(gap = year_files - dplyr::lag(year_files))) %>%
    purrr::map(~ .x %>% filter(gap > 1))

  # Attribute country name to each element of the list:

  names(list_year_files) <- names_individual


  # Explicit the year gap - write into character:
  # if statement to filter only for big gaps (more than one year)

  if (only_large == T) {
    gap_character <- list_year_files %>%
      purrr::map(~ .x %>% mutate(large_gap = case_when(
        gap > 2 ~ "Yes",
        TRUE ~ "No"
      ))) %>%
      purrr::map(~ .x %>% filter(large_gap == "Yes")) %>%
      purrr::map(~ .x %>% mutate(gap = paste0(year_files - gap, "-", year_files))) %>%
      purrr::map(~ .x %>% select(gap, large_gap))
  } else {
    gap_character <- list_year_files %>%
      purrr::map(~ .x %>% mutate(large_gap = case_when(
        gap > 2 ~ "Yes",
        TRUE ~ "No"
      ))) %>%
      purrr::map(~ .x %>% mutate(gap = paste0(year_files - gap, "-", year_files))) %>%
      purrr::map(~ .x %>% select(gap, large_gap))
  }

  # Provide a final list with number of gaps by country and the dataframe with detail:

  gap_character %>%
    purrr::discard(~ nrow(.x) == 0) %>% # remove countries with gaps from list
    purrr::map(~ list(gap_number = nrow(.x), gap_detail = .x))
}


add_to_corpus <- function(old_corpus_path, files_updated_path, ENGINE) {
  #' Add new files to a corpus
  #' Add new files to a corpus without reaggregating everything
  #' @param old_corpus_path path to old corpus. List
  #' @param files_updated_path directory with some new pdfs.
  #' @param ENGINE similar to engine argument in readPDF from 'tm' package.
  #' Function to read pdf into environment, either pdf_text or pdf_ocr_text, depending on whether image or not.
  #' @author Manuel Betin, Umberto Collodel
  #' @noRd
  old_corpus <- rio::import(old_corpus_path)
  stopifnot(is.list(old_corpus))

  # Find files not included in the old corpus:

  names_prexisting <- names(old_corpus)
  names_new <- list.files(files_updated_path) %>%
    stringr::str_remove(".pdf")

  names_new_docs <- setdiff(names_new, names_prexisting)

  # Aggregate the new files:

  count <- 0

  new_corpus <- names_new_docs %>%
    map_chr(~ paste0(new_files_path, "/", .x, ".pdf")) %>%
    map(function(x) {
      count <<- count + 1
      tictoc::tic(paste0(count, "/", length(names_new_docs), " ", x))
      file <- tryCatch(ENGINE(x), error = function(e) {
        cat(crayon::red("Error aggregating new document: ", x, "\n"))
      })
      tictoc::toc()
      clean_text(file)
    })

  # Assign names:

  names(new_corpus) <- names_new_docs

  # Add elements to the list and sort by element name:

  final_corpus <- c(new_corpus, corpus)
  final_corpus <- final_corpus[order(names(final_corpus))]

  # Change class:

  attr(final_corpus, "class") <- c("corpusTM", "list")
  return(final_corpus)
}


check_diff_pdfs_urls <- function(path, urls_data) {
  #' Check difference between pdfs downloaded and original urls
  #' Check difference between pdfs downloaded and original urls
  #' @param path path to all individual units subdirectories.
  #' @param urls_data dataframe. original urls, must have at least a column named "name_file".
  #' @author Manuel Betin, Umberto Collodel
  #' @noRd
  # List of pdfs downloaded:

  tot_list_pdfs <- list.files(path) %>%
    str_subset("[A-Z]{3}") %>%
    map_chr(~ paste0(path, .x, "/files")) %>%
    map(~ list.files(.x)) %>%
    modify_depth(2, ~ .x %>% str_remove(".pdf")) %>%
    reduce(`c`)

  # List of name files from URLs database:

  if ("name_file" %in% names(urls_data)) {
    tot_list_urls <- urls_data %>%
      select(name_file) %>%
      .$name_file
  }
  else {
    cat(crayon::red("The url dataframe supplied has no column 'name_file'"))
  }

  # Comparison:

  setdiff(tot_list_urls, tot_list_pdfs)
}
