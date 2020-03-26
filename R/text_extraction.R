
pdf_from_url = function(urls, export_path, overwrite = T) {
  #' download pdf documents 
  #' @description  download from a a dataframe containing the url of the files
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
  #' @description count the number of pages in the pdf
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

aggregate_corpus = function(path_files, ENGINE=pdf_text, only_files = F) {
  #' Aggregate pdf files into list of characters
  #'
  #'@description function that takes the path of the directory and load all
  #' the pdfs of the directory into a list in order to further perform the text
  #' mining
  
  #' @param path_files the path of the directory with the files
  #' @param ENGINE similar to engine argument in readPDF from 'tm' package. 
  #' Function to use to read pdf into environment, either pdf_text or pdf_ocr_text.
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
      ENGINE(path) %>% strsplit(split = "\n")
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
  #' @description Provide list of files and return summary of counts of occurence of the
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
  #' @description Provide files either pdf of html and return the paragraphs matching the
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
  #'@description Function that simplifies checking validity indexes and potential problems in keywords
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
  #' @description log normal transformation of the table
  #' @param tf_data a dataframe of term frequencies
  #' @return a tibble of tf with log norm transformation
  #' @author Manuel Betin
  log_norm_trans = function(x) {
    ifelse(x > 0, 1 + log(x), 0)
  }
  table_log_norm_trans = tf_data %>% dplyr::mutate_if(is.numeric, 
                                                      log_norm_trans)
  return(table_log_norm_trans)
}

binary_freq_trans = function(tf_data) {
  #' binary transformation of tfidf
  
  #'@description  transform table from Number of occurence to binary variables
  
  #' @param tf_data a dataframe with numerical columns corresponding
  #' to the tf idf of each category
  #' @author Manuel Betin
  #' @return a dataframe with binary frequencies 0 if the tf_idf is zero and 1
  #' if it is non zero
  
  binary_trans = function(x) {
    ifelse(x > 0, 1, 0)
  }
  table_brut_frequency = tf_data %>% dplyr::mutate_if(is.numeric, 
                                                      binary_trans)
  return(table_brut_frequency)
}


check_extract=function(path_urls="../Betin_Collodel/2. Text mining IMF_data/datasets/urls docs/urls_Requests_Reviews_articleIV.RData",
                       path_tf_idf="../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf.RData",
                       path_final_tf_idf="../Betin_Collodel/2. Text mining IMF_data/datasets/tagged docs/tf_idf_database.RData"){
  
  #' find attrition in the NLP process
  #'@description check the validity of each steps in the scripts to see what documents are lost
  #'in each step of the process from the download, to the 
  #'text mining and finally to the merging with the rest of the variables
  
  #'@param path_urls the path to locate the dataframe of urls (output of 1. Consolidate_urls.R)
  #'@param path_tf_idf: path to locate the tf_idf matrix (output of 2. Run_text_mining.R)
  #'@param path_final_tf_idf: path to locate the final database containing the tf of the text mining and the
  #' rest of the variables merge after (output of 3. Clean_database.R)
  #'@author Manuel Betin
  #'@return a list with a table given the proportion of the files that have not been properly computed by country
  #' the files that have an error and have not been downloaded and the files that have not been downloaded and
  #' merged
  #' 
  #'@export
  
  metadata=rio::import(path_urls) %>%
    mutate(name_file=paste0(ID,"_",period,"_",type_doc_programs)) 
  vars_metadata=names(metadata)
  size_metadata=dim(metadata)
  name_file_metadata=metadata$name_file %>% unique()
  
  #tf_matrix after running the text mining
  tf=rio::import(path_tf_idf)
  vars_tf=names(tf) 
  size_tf=dim(tf)
  name_file_tf=tf$file %>% unique()
  
  #tf_idf matrix combining all the available data
  final_data=rio::import(path_final_tf_idf)
  vars_final_data=names(final_data) 
  size_final_data=dim(final_data)
  name_file_final_data=final_data$file %>% unique()
  
  lost_files_during_textmining=setdiff(name_file_metadata,name_file_tf)
  missing_urls=metadata %>% filter(name_file %in% lost_files_during_textmining)
  
  error_in_url= missing_urls %>% filter(ID %in% tf$ISO3_Code)
  #proportion of documents properly downloaded
  prop_error_in_url=metadata %>%
    mutate(missing_urls=ifelse(name_file %in% lost_files_during_textmining,1,0)) %>% group_by(ID) %>% mutate(N_tot_docs=n())%>%ungroup()%>%
    group_by(ID,missing_urls) %>% summarize(Prop_missing_urls=1-n()/first(N_tot_docs)) %>% filter(missing_urls==0) %>% ungroup() %>% dplyr::select(-missing_urls)
  
  return(list(proportion_error_in_url=prop_error_in_url,
              files_with_error_in_url=error_in_url,
              files_not_downloaded=missing_urls))
}


get_imf_country_reports=function(npages=NULL){
  #' download the url of the country reports from the imf website
  #' @description Provide the urls to download the imf country reports from
  #' https://www.imf.org/en/Publications/CR/
  #' @param npages the number of pages to download by default it wil download all the pages 
  #' that he will find. to download only the most recent set npages=1
  #' @author Manuel Betin
  #' @return dataframe with the title of the document, the name of the file and the url of the file
  #' @export 
  #' 
  #' 
  
  #find the hompage of the IMF
  homepage=read_html("https://www.imf.org/en/Publications/CR/")
  #find the total number of pages of country reports
  if(is.null(npages)){
    npages=homepage %>% html_nodes(xpath = "/html/body/div[3]/main/article/div[3]/div[1]/p/text()") %>% html_text() 
    npages=str_extract(npages,"\\d\\d\\d")[2] %>% as.numeric()
    cat(crayon::blue(paste("Download of all the country reports,",npages,"pages in total", sep = "")))
  }else{
    cat(crayon::blue(paste("Download only the last,",npages,"pages (",npages*10,") documents", sep = "")))
  }
  progress = dplyr::progress_estimated(npages)
  urls=lapply(1:npages,function(x){
    tictoc::tic(paste0("page: ",x,"/",npages))
    #get pages of country report
    homepage=read_html(paste0("https://www.imf.org/en/Publications/CR/?page=",x))
    results=homepage %>% html_nodes(xpath = '/html/body/div[3]/main/article/div[3]')
    #get url of the page of the document
    href=results %>% html_nodes("a") %>% html_attr("href")
    href=data.frame(href) 
    href=href %>% filter(!str_detect(href,"www.imf.org")) %>%
      mutate(url=paste0("https://www.imf.org/",href))
    #get url of the pdf and metadata associated
    dt=lapply(href$url,function(myurl){
      dt=try({ 
        #get the html of the apge
        docpage=read_html(myurl)
        #find the name of the document
        docname=docpage %>% html_nodes(css=".conf") %>% html_text()
        #find the date of the document and clean 
        docdate=docpage %>% html_nodes(css=".pub-lang .pub-desc") %>% html_text()
        docdate=docdate[1]
        docdate=str_remove_all(docdate,"\r\n") %>% str_remove_all(" ")
        #find url to download pdf
        docurl= docpage %>% html_nodes(css = ".piwik_download") %>% html_attr('href')
        docurl=paste0("https://www.imf.org",docurl)
        #consolidate info
        data.frame(title=docname,period=docdate,pdf=docurl)}, silent = T)
      if ("try-error" %in% class(dt)) { #on error return NA
        cat(crayon::red(paste(docname, ": Error in path file: ", 
                              myurl, sep = "")))
        data.frame(title=NA,period=NA,pdf=myurl)
      }
      dt
    })
    dt=do.call(rbind,dt)
    dt=dt %>% mutate(pdf=as.character(pdf),
                     name_file=str_extract(pdf,"[:alpha:][:alpha:][:alpha:][:alpha:][:alpha:]\\d+(?=.ashx?)"),
                     iso3=substr(name_file,1,3),
                     period=as.Date(period,format=c("%B%d,%Y")),
                     year=year(period)) %>% dplyr::select(title,pdf,iso3,period,year,name_file)
    progress$pause(0.01)$tick()$print()
    tictoc::toc()
    dt
  })
  urls=do.call(rbind,urls)
  
  #clean the titles to find the iso code
  find_name_from_title=function(dt){
    
    if(!any(c("title","year") %in% names(dt))){
      print("please provide a valide database containing at least the columns title and year")
      dt
    }else{
      dt=dt %>% mutate(title2=str_replace(title,":","-")) %>% separate(title2,into="country",sep="-") %>% dplyr::select(iso3,country,period,title,everything())
      
      dt= dt  %>% mutate(country=str_trim(gsub('[^ -~]', '', country),"both"))
      
      ctries=countrycode::countrycode(list_countries(),origin="iso3c",destination="country.name") %>% tolower()
      
      nonstandard_ctrynames=c(COD="zaire",SOM="somalia",YEM="yemen arab republic","yugoslavia",CIV="ivory coast",WSM="western samoa",HUN="hungarian people's republic",KOR="korea",
                              MMR="burma",VCT="st. vincent and the grenadines",GMB="the gambia",CIV="cote d'ivoire",COD="people's republic of the congo",CHN="people's republic of china",
                              EGY="arab republic of egypt",MOZ="people's republic of mozambique",TTO="trinidad and tobago",STP="sao tome and principe",LAO="lao people's democratic republic",
                              MOZ="republic of mozambique",POL="republic of poland",CZE="czech and slovak federal republic",RUS='russian federation',CZE="czech republic",SVK="slovak republic",
                              LVA='republic of latvia',KGZ="kyrgyz republic",MDA="republic of moldova",VNM="viet nam",LTU="republic of lithuania",EST="republic of estonia",KAZ="republic of kazakhstan",
                              MKD="former yugoslav republic of macedonia",COG="republic of congo",HRV="republic of croatia",ARM="republic of armenia",BLR="republic of belarus",UZB="republic of uzbekistan",
                              AZE="azerbaijan republic",GEO="republic of georgia",KAZ="republic of kazakstan",BIH="republic of bosnia and herzegovina",YEM="republic of yemen",TJK="republic of tajikistan",
                              BIH="bosnia and herzegovina",KOR="republic of korea",KNA="st. kitts and nevis",GNQ="guinea bissau",MEX="mexico <U+0097> arrangement under the flexible credit line",
                              MEX="mexico<U+0097>review under the flexible credit line arrangement",COL="colombia<U+0097>review under the flexible credit line arrangement")
      
      nonstandard_ctrynames2=as.data.frame(nonstandard_ctrynames)
      nonstandard_ctrynames2$iso3c=names(nonstandard_ctrynames)
      names(nonstandard_ctrynames2)=c("iso3_new","iso3c")
      nonstandard_ctrynames2=nonstandard_ctrynames2 %>% mutate(iso3_new=as.character(iso3_new))
      
      dt=dt %>% mutate(iso3_error=ifelse(!country %in% c(ctries,nonstandard_ctrynames),country,""),
                       iso3_new=as.character(ifelse(country %in% c(ctries,nonstandard_ctrynames),country,"")))
      
      dt=dt %>% left_join(nonstandard_ctrynames2,by=c("iso3_new"))
      
      #correct manually some cases and transform to iso3c
      dt=dt %>% mutate(iso3c=ifelse(is.na(iso3c),countrycode::countrycode(iso3_new,origin="country.name",destination="iso3c"),iso3c),
                       iso3c=ifelse(str_detect(iso3,"mexico"),"MEX",iso3c),
                       #iso3c=ifelse(str_detect(title,"germany"),"DEU",iso3c),
                       iso3c=ifelse(str_detect(iso3,"philippines"),"PHL",iso3c),
                       iso3c=ifelse(str_detect(iso3,"macedonia"),"MKD",iso3c),
                       iso3c=ifelse(str_detect(iso3,"yugoslavia"),"YUG",iso3c))
      
      
      mycountries=c(ctries,nonstandard_ctrynames)
      for(j in 1:length(mycountries)){
        iso3ccode=countrycode::countrycode(mycountries[j],origin="country.name",destination="iso3c")
        dt=dt%>%mutate(iso3c=ifelse(is.na(iso3c) & str_detect(title,mycountries[j]),iso3ccode,iso3c))
      }
      
      
      
      dt=dt %>% dplyr::select(-c(iso3_new,iso3_error)) %>% rename(iso3_from_title=iso3c) %>%
        dplyr::select(iso3,country,iso3_from_title,period,year,pdf,everything())
      
      
    }
    dt
  }
  
  urls=urls %>% find_name_from_title()
  urls= urls %>% mutate(iso3=countrycode::countrycode(country,origin="country.name",destination="iso3c"))
  urls=urls %>% mutate(title=tolower(title)) %>% dplyr::select(title,pdf,iso3,period,year,name_file)
  
  return(urls)
}
