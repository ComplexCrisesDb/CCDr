tf = function(corpus, keywords, brute_freq = F, parrallel = T) {
  #' Compute the term freqeuency matrix
  #' for each document in the corpus the function creates a table counting the
  #' occurence of the words in the vector of keywords and provide a table with
  #' the number of occurence, name of the file that is the name of each
  #' document in the corpus
  
  #' @param corpus a list of texts from pdf_from_url() with different names
  #' for each element
  #' @param  keywords a vector of strings containing the targeted
  #' words to look for
  #' @param brute_freq T/F if set to F it will compute the term frequency
  #' otherwise it will only count the occurence
  #' @param parrallel T/F if T use mclapply from parrallel package
  #' @author Manuel Betin
  #' @return a dataframe of term frequencies with documents in rows and categories
  #' in columns
  #' @export
  
  if (is.vector(keywords) & !is.null(keywords)) {
    table = lapply(corpus, function(x) {
      
      # Exclude minutes meetings, press releases and working papers erroneously scraped.
      # Check the first page of each document for bad keyword.
      
      valid_document <- tibble(first_page = tolower(x[[1]])) %>% 
        filter(!str_detect(first_page, paste(lexicon()[["Problematic_documents"]], collapse = "|"))) %>% 
        nrow()
      
      # If is a valid document, continue with tf-calculation. Else, return NA.
      
      if (valid_document == 1){
        output = try(sum(eval_pages(x, keywords, brute_freq = brute_freq, 
                                    parrallel = parrallel)[, 1]))
        if ("try-error" %in% class(output)) {
          cat(crayon::red(paste0("Warning: error when mining ", keywords)))
          output = 0
        }
        output
      }
      else { 
        NA
      }
    })
    names(table) = names(corpus)
    table = do.call(rbind, table)
    table = dplyr::tibble(table)
    table$file = names(corpus)
    names(table) = c("var", "file")
    return(table)
  } else {
    cat(crayon::red("please provide a vector of strings as argument for keywords"))
  }
}

tf_vector = function(corpus, keyword_list, brute_freq = F, parrallel = T, centre_countries = "USA") {
  #' vectorize the function tf() to be able to pass a list of names of keywords
  #' keyword_list is a list containing the names of different groups of
  #' keywords that have a vector of words to look into.
  
  #' @param corpus a list of texts from pdf_text() with different names
  #' for each element
  #' @param keyword_list the names of the items in lexicon to
  #'  include in the computation
  #' @param brute_freq T/F if T it will just count the occurence, otherwise
  #'  it will compute the term frequency
  #' @param parrallel T/F if T it will use mclapply from the parrallel package
  #' @param centre_countries Character string. Default is "USA". Netting of confusing
  #' keywords will not be performed countries in the vector.
  #'  @author Manuel Betin
  #'  @return a tibble with the term frequencies for the selected categories
  #'  @export
  
  progress = dplyr::progress_estimated(length(keyword_list))
  list_table_keyword_occurence = lapply(1:length(keyword_list), function(x) {
    cat(crayon::bgBlack("\n"))
    cat(crayon::green(paste0("(", x, "/", length(keyword_list), ") running: ", 
                             names(keyword_list)[x], "\n")))
    tictoc::tic(names(keyword_list)[x])
    if (!"character" %in% class(keyword_list[[x]])) {
      warning("please provide a valid vector of characters")
      dt = NULL
      tictoc::toc()
      dt
    } else {
      res = try({
        dt = tf(corpus, keyword_list[[x]], brute_freq = brute_freq, 
                parrallel = parrallel)
        dt = dt %>% dplyr::rename(`:=`(!!paste0(names(keyword_list)[x]), 
                                       var))
        dt
      })
      if ("try-error" %in% class(dt)) {
        cat(crayon::red(paste0("\n Error: term frequency as not been computed", 
                               "\n")))
        res = NULL
      }
      tictoc::toc()
      progress$pause(0.01)$tick()$print()
      cat(crayon::green(paste0("\n Finished running: ", names(keyword_list)[x], 
                               "\n")))
      res
    }
  })
  names(list_table_keyword_occurence) = names(keyword_list)
  
  
  dt = list_table_keyword_occurence[[1]]
  
  # If more than one index, reduce the merge.
  
  if(length(names(keyword_list)) > 1){
    for (i in 2:length(list_table_keyword_occurence)) {
      res = try({
        dt = dt %>% dplyr::left_join(list_table_keyword_occurence[[i]], 
                                     by = c("file"))  #'ISO3_Code','Period','type_prog','year',  
        dt
      })
      if ("try-error" %in% class(res)) {
        dt = dt
      } else {
        dt = res
        dt = dt %>% dplyr::select(file, everything()) %>% dplyr::distinct()  #ISO3_Code,Period,year,type_prog,
      }
    }
  }
  
  # At this stage dataframe with file name and all different indexes.
  # Include additional step to net out index computed on confusing keywords (e.g. referring to other countries):
  
  # List of "confusing" categories on which the index could be computed:
  
  list_net_keywords <- str_extract(names(lexicon()), ".+_confusing")[complete.cases(str_extract(names(lexicon()), ".+_confusing"))]
  
  # Double check: check if we calculated some and check if country different from centre countries. If TRUE,
  # proceed to net. Otherwise return df.
  
  if (any(list_net_keywords %in% names(dt)) & unique(str_extract(dt$file,"[A-Z]{3}") != centre_countries)){
    
    dt = split.default(dt, str_remove(names(dt), "_.+"))  # split into list according to the first word of column name, removing the rest.
    
    dt = dt %>%    
      purrr::map(~ if (any(names(.x) != "file")) {                               
        if (any(str_detect(names(.x), "confusing"))) {                               # if column name with "confusing" within same category 
          colnames(.x)[grepl('confusing',colnames(.x))] <- "confusing"            # subtract from other indexes.
          .x %>% 
            mutate_all(funs(. - confusing)) %>% 
            select(-confusing)
        } else {
          .x
        }
      }
      else {
        .x
      }) 
    
    
    dt %>%                                          # Bind net indexes with file list.
      purrr::reduce(cbind) %>% 
      select(file,everything())
    
  } else {
    dt
  }
}

tf_by_group = function(tf_data, weight_method = "brut_frequency", 
                       mygroup = NULL) {
  
  #' Summarize the term frequency matrix by group
  #' Summarize the tf matrix according to the grouping variable selected
  
  #' @param tf_data a tibble of term frequencies from tf(), run_tf(),
  #'  run_tf_update() or run_tf_by_chunk()
  #' @param  weight_method the method for countring "brut_frequency", "binary_frequency" 
  #' @param mygroup the grouping variable to be used for the summary
  #' @author Manuel Betin
  #' @return a dataframe of term frequencies with documents in rows and categories
  #' in columns
  #' @export
  #' 
  if (is.null(mygroup)) {
    avoid_colums = c(names(tf_data %>% dplyr::select_if(is.character)), 
                     names(tf_data %>% dplyr::select_if(is.Date)))
    dt_weights_years = tf_data %>% tidyr::gather("Crisis", value = "word_weight", 
                                                 -c(avoid_colums))
    dt_weights_years = dt_weights_years[-1, ]
    dt_weights_years = dt_weights_years %>% dplyr::mutate(word_weight = as.numeric(word_weight))
    return(dt_weights_years)
  } else {
    avoid_colums = c(names(tf_data %>% dplyr::select_if(is.character)), 
                     names(tf_data %>% dplyr::select_if(is.Date)))
    
    dt_weights_years = tf_data %>% tidyr::gather("Crisis", value = "word_weight", 
                                                 -c(avoid_colums))
    dt_weights_years = dt_weights_years[-1, ]
    dt_weights_years = dt_weights_years %>% group_by(get(mygroup)) %>% dplyr::mutate(word_weight = as.numeric(word_weight))
    return(dt_weights_years)
  }
}


run_tf = function(corpus_path = "IMF_letofIntent_1960_2014_clean.RData", 
                  type_lexicon = "words", 
                  keyword_list = c("Commodity_crisis", 
                                   "Balance_payment_crisis",
                                   "Inflation_crisis"), 
                  export_path = NULL, parrallel = T) {
  
  #' Compute the tf matrix for a corpus given in a list format,
  #' @description the type of lexicon to use and the sublist of keywords associated The
  #' output is a matrix of tf with a row per document and a column for each
  #' element of the keyword list
  #' @param corpus_path the path to the RData file containing the corpus to analyze
  #' @param type_lexicon "words" or "categories" for the type of lexicon to use
  #' @param keyword_list the categories on which computing the term frequency
  #' @param export_path the path were to export the tf
  #' @param parrallel T/F to use mclapply from the parrallel package
  #' @return a dataframe of term frequencies
  #' @author Manuel Betin
  #' @export
  
  cat(crayon::bgBlue(paste0("Loading corpus from ", corpus_path)))
  corpus = rio::import(corpus_path)
  if (type_lexicon == "words") {
    if (is.null(keyword_list)) {
      keyword_list = c("Commodity_crisis", "Balance_payment_crisis", "Inflation_crisis", 
                       "World_outcomes", "Floating_Exchange rate", "Fixed_Exchange rate", 
                       "Wars", "Soft_Recession", "Fiscal_Outcomes", "Financial_crisis", 
                       "Banking_crisis", "Currency_crisis", "Severe_recession", "Crisis_contagion", 
                       "Sovereign_default", "Expectations", "Precautionary_programs", 
                       "Natural_desaster", "Loss_Market_access", "Political_crisis", 
                       "Constraining", "Contagion")
      print(keyword_list)
    }
    keyword_list = lexicon()[keyword_list]
  } else if (type_lexicon == "category") {
    if (is.null(keyword_list)) {
      keyword_list = c("Exogenous", "Manifestations", "Instruments")
    }
    keyword_list = key_words_categories()[keyword_list]
  } else {
    cat(crayon::red("Please provide a valid type_lexicon, either 'words' or 'category'"))
    return(NULL)
  }
  
  tictoc::tic()
  dt = tf_vector(corpus, keyword_list, parrallel = parrallel)
  tictoc::toc()
  destination = paste0(export_path, "/tf_crisis_", type_lexicon, ".RData")
  print(paste0("export table in ", corpus_path))
  if (!is.null(export_path)) {
    rio::export(dt, destination)
  }
  return(dt)
}



run_tf_update = function(path_tf_to_update = "tf_crisis_words.RData", 
                         corpus_path = "IMF_letofIntent_1960_2014_clean.RData", 
                         type_lexicon = "words", keyword_list = NULL, 
                         export_path = "tf_crisis_words.RData", 
                         parrallel = T, store_old = F, store_old_path = NULL) {
  #'Updates the tf-indexes without repeating full extraction
  #'
  #' @description Combine probability of shocks, intensity and complexity of relations to construct
  #' .a mesure of severity of crisis
  #'
  #' @param path_tf_to_update Path to old tf dataframe.
  #' @param corpus_path Path to corpus from which perform the extraction.
  #' @param type_lexicon Character: "words" or "category"
  #' @param keyword_list Character vector: names of character vectors to use for extraction.
  #' @param export_path Path to export the file.
  #' @param parrallel Logical. If TRUE, parallel computation for each category.
  #' @param store_old Logical. If TRUE, store old extractions in a directory.
  #' @param store_old_path Path to move old extractions in.
  #'
  #' @return A dataframe with file name, old indexes not specified in keyword_list and updated indexes.
  #'
  #' @author Manuel Betin, Umberto Collodel
  #'
  #' @examples
  #'
  #'
  #' @export
  
  if (is.null(keyword_list)) {
    print("Updating all columns")
    new_tf = run_tf(corpus_path = corpus_path, type_lexicon = type_lexicon, 
                    keyword_list = lexicon(), export_path = paste0(root_path, 
                                                                            "/3. Data/IMF Letters of Intents/tf_crisis_words.RData"), parrallel = parrallel)
    return(new_tf)
  } else {
    
    cat(crayon::green("updating selected columns"))
    tf_to_update = rio::import(path_tf_to_update)
    dim_tf_to_update = dim(tf_to_update)
    existing_cols = names(tf_to_update)
    
    if (any(existing_cols %in% keyword_list)) {
      # Remove only columns in both update list and old dataframe:
      existing_keyword_list <- intersect(existing_cols, keyword_list)
      tf_to_update = tf_to_update %>% dplyr::select(- existing_keyword_list)
    }
    
    corpus = rio::import(corpus_path)
    
    new_tf = run_tf(corpus_path = corpus_path, type_lexicon = type_lexicon, 
                    keyword_list = keyword_list, parrallel = parrallel)
    
    tf_to_update = dplyr::left_join(x = tf_to_update, y = new_tf, by = "file")
    
    print(paste0("Non updated columns:\n
                 ", paste0(existing_cols, 
                           collapse = ", ")))
    
    print(paste0("Updated columns:\n
                 ", paste0(keyword_list, 
                           collapse = ", ")))
    
    if(store_old == T){
      if(dir.exists(store_old_path) != T){
        dir.create(store_old_path)
      }
      # Move old files:
      file.move(path_tf_to_update, store_old_path, overwrite = TRUE)
    }
    
    rio::export(tf_to_update, export_path)
    
    return(tf_to_update)
  }
  
  
}


run_tf_by_chunk=function (urls = url_links, keyword_list = c("Fiscal outcomes", 
                                                             "Currency_crisis"),
                          extract_number = 1,
                          ENGINE=pdf_text,
                          delete_pdfs = T,
                          rm_short_docs=F,
                          min_words=100,
                          parrallel=T,
                          loc_temp=NULL) 
{
  
  #' run the term frequency matrix on the list of urls provided as parameter.
  #' @description The function download the pdf, create the corpus and generate the term
  #' frequency matrix, to avoid storage limitation the function deleate
  #' original pdf files
  
  #' @param urls a dataframe containing two columns: name_file and pdf
  #' that respectively provid the name that will be given to the file
  #' downloaded and pdf that contains the url of the pdf
  #' @param keyword_list one of the element of the list provided by
  #'  lexicon()
  #' @param extract_number a number that will be ued as a suffix for the name 
  #' of the corpus and the name of the tf matrix
  #' @param ENGINE similar to engine argument in readPDF from 'tm' package. 
  #' Function to use to read pdf into environment, either pdf_text or
  #' pdf_ocr_text.
  #' @param delete_pdfs T/F, set to T it will delete the folder 
  #' containing the original pdf, usefull option when the number of pdf is very
  #' large and the size of the folder start to be very large
  #' @param ENGINE function to read pdf into environment. Either pdf_text or pdf_ocr_text.
  #' @param rm_short_docs T/F T if you want to remove the documents under a 
  #' certain number of words
  #' @param min_words the minimum word in the document necessary to perform
  #' the text mining
  #' @param parrallel if T then will use mclapply from the package parrallel
  #' @param loc_temp   path to the folder containing the files, corpus and tf
  #' @author Manuel Betin
  #' @return create forlders where pdfs, corpus and tf-idf matrix are stored
  #' @export
  #' 
  if(is.null(loc_temp)){
    path = "temp"  
  }else{
    path=loc_temp
  }
  
  path_pdf_files = paste0(path, "/files")
  path_corpus = paste0(path, "/corpus")
  path_tf = paste0(path, "/tf")
  
  dir.create(path)
  dir.create(path_pdf_files)
  dir.create(path_corpus)
  dir.create(path_tf)
  
  # download the files
  pdf_from_url(urls, path_pdf_files, overwrite = F)
  # transform pdf to character and store in list
  corpus = aggregate_corpus(path_pdf_files,ENGINE=ENGINE,only_files=T)
  
  # remove documents with less than specified number of words
  if(rm_short_docs){
    cat(crayon::blue(paste0("\n Remove from corpus the documents with less than ",min_words," words\n")))
    N_char_corpus=sapply(1:length(corpus),function(x){
      sum(stri_count_words(corpus[[x]]))
    })
    names(N_char_corpus)=names(corpus)
    N_char_corpus=data.frame(N_char_corpus)
    N_char_corpus$file=names(corpus)
    N_char_corpus=N_char_corpus %>% filter(N_char_corpus>=min_words)
    corpus=corpus[N_char_corpus$file]
  }
  
  # save corpus
  save(corpus, file = paste0(path_corpus, "/corpus_", extract_number, 
                             ".RData"))
  
  # delete folder with pdf after consolidating
  cat(crayon::blue("delete folder with pdf \n"))
  if (delete_pdfs) {
    unlink(path_pdf_files, recursive = T)
  }
  # run the term frequency matrix
  dt = run_tf(corpus_path = paste0(path_corpus, "/corpus_", 
                                   extract_number, ".RData"), type_lexicon = "words", keyword_list = keyword_list, 
              export_path = path_tf, parrallel = parrallel)
  
  file.rename(from = paste0(path_tf, "/tf_crisis_words.RData"), 
              to = paste0(path_tf, "/tf_crisis_words_", extract_number, 
                          ".RData"))
}


idf = function(tf_data, group = NULL) {
  #' Compute the inverse document frequency
  #' @description The idf is computed as the logarithm of the inverse of the proportion. 
  #' it allows to give reduce weight of words with high
  #' frequency in the corpus
  #' @param tf_data a dataframe of term frequencies
  #' @param group the grouping variable 
  #' @return a tibble with the idf of each categories 
  #' @author Manuel Betin
  #' @export
  
  N.doc.corpus = dim(tf_data)[1]
  idf_trans = function(x) {
    if (x == 0) {
      x
    } else {
      log(N.doc.corpus/sum(x))
    }
  }
  
  table_N_binary = binary_freq_trans(tf_data)
  
  if (!is.null(group)) {
    inverse_doc_freq = table_N_binary %>% dplyr::group_by(get(group)) %>% 
      dplyr::summarize_if(is.numeric, sum) %>% dplyr::mutate_if(is.numeric, 
                                                                idf_trans) %>% gather("Crisis", value = "idf", -"get(group)")
    colnames(inverse_doc_freq)[1] = group
  } else {
    inverse_doc_freq = table_N_binary %>% dplyr::summarize_if(is.numeric, 
                                                              sum) %>% dplyr::mutate_if(is.numeric, idf_trans) %>% gather("Crisis", 
                                                                                                                          value = "idf")
  }
  return(inverse_doc_freq)
}

tf_idf = function(tf_data, weight_method = "brut_frequency") {
  #' Compute the tf-idf matrix
  #' @description weight the term frequency (tf) of each category by the its idf to
  #' improve the discriminatory power of the categories that are rare in
  #' the corpus
  
  #' @param tf_data the tf matrix from tf()
  #' @param  weight_method the method for the counting: "binary_frequency" or "brut_frequency"
  #' @author Manuel Betin
  #' @return a dataframe of tf-idf with documents in rows and categories
  #' in columns
  #' @export
  #' 
  #' 
  dt_inv_doc_freq = try(idf(tf_data))
  if ("try_error" %in% dt_inv_doc_freq) {
    cat(crayon::red(paste0("Warning: error when using function dt_inv_doc_freq")))
    return(NULL)
  }
  select_cols = names(tf_data %>% dplyr::select_if(is.numeric))
  
  if (weight_method == "brut_frequency") {
    dt_words_weight = tf_data
    for (var in select_cols) {
      dt_words_weight[, var] = dt_words_weight[, var] * (dt_inv_doc_freq %>% 
                                                           dplyr::filter(Crisis == var) %>% dplyr::select(idf))[[1]]
    }
    return(dt_words_weight)
  } else if (weight_method == "binary_frequency") {
    dt_words_weight = binary_freq_trans(tf_data)
    for (var in select_cols) {
      dt_words_weight[, var] = dt_words_weight[, var] * (dt_inv_doc_freq %>% 
                                                           dplyr::filter(Crisis == var) %>% dplyr::select(idf))[[1]]
    }
    return(dt_words_weight)
  } else if (weight_method == "log_norm_frequency") {
    dt_words_weight = log_norm_trans(tf_data)
    for (var in select_cols) {
      dt_words_weight[, var] = dt_words_weight[, var] * (dt_inv_doc_freq %>% 
                                                           dplyr::filter(Crisis == var) %>% dplyr::select(idf))[[1]]
    }
    return(dt_words_weight)
  } else warning("please choose a proper method: brut_frequency,binary_frequency,log_norm_frequency")
}



tf_barplot = function(tf, vars_type = c("economic_shock", "non_economic_shock", 
                                        "debt_outcomes", "debt_structure", 
                                        "characteristics_program", "adustment_program"), 
                      vars_nature = NULL) {
  #' barplot of term frequencies
  #' @description take as input the term frequency matrix and reshape it to plot the values
  #' for all numeric columns and group them in two dimensions: the type of
  #' variable and the nature of the shock (exogeneous or endogenous)
  #' @param tf a term frequency matrix from tf()
  #' @param vars_type the type of categories to include (see lexicon_typology()
  #'  for more details)
  #' @param vars_nature the nature of categories to include (see lexicon_typology()
  #'  for more details)
  #' @author Manuel Betin
  #' @return a ggplot barplot
  #' @export
  
  res = list()
  avg_tf = tf %>% mutate_if(is.numeric, funs(ifelse(. == 0, NA, .))) %>% ungroup() %>% 
    summarize_if(is.numeric, funs(mean(., na.rm = T) * 100)) %>% gather(key = "variable", 
                                                                        value = "tf_idf")
  
  avg_tf = avg_tf %>% left_join(lexicon_typology(), by = c("variable"))
  
  res[["tf_table_avg"]] = avg_tf
  
  if (!is.null(vars_nature)) {
    avg_tf = avg_tf %>% filter(type %in% vars_type & nature_shock %in% vars_nature)
  } else {
    avg_tf = avg_tf %>% filter(type %in% vars_type)
  }
  
  avg_tf = avg_tf %>% mutate(variable = str_replace_all(variable, "_", " "))
  prop_avg_tf = avg_tf %>% mutate(tf_idf = tf_idf/sum(tf_idf))
  
  if (!is.null(vars_nature)) {
    res[["tf_fig_avg"]] = ggplot(avg_tf) + geom_bar(stat = "identity", aes(x = reorder(variable, 
                                                                                       tf_idf), y = tf_idf, fill = type)) + geom_text(aes(x = reorder(variable, 
                                                                                                                                                      tf_idf), y = tf_idf * 0.8, label = substr(nature_shock, 1, 3)), 
                                                                                                                                      color = "white") + scale_fill_manual(values = c("darkred", "blue", 
                                                                                                                                                                                      "darkgreen", "lightgrey", "orange", "brown")) + theme_bw() + labs(x = NULL, 
                                                                                                                                                                                                                                                        y = "Term frequency (%)") + theme(legend.title = element_blank(), 
                                                                                                                                                                                                                                                                                          legend.position = "bottom", axis.text.x = element_text(angle = 90, 
                                                                                                                                                                                                                                                                                                                                                 hjust = 1), axis.text = element_text(size = 8))
  } else {
    res[["tf_fig_avg"]] = ggplot(avg_tf) + geom_bar(stat = "identity", aes(x = reorder(variable, 
                                                                                       tf_idf), y = tf_idf, fill = nature_shock)) + scale_fill_manual(values = c("darkred", 
                                                                                                                                                                 "blue", "darkgreen", "lightgrey", "orange", "brown")) + theme_bw() + 
      
      labs(x = NULL, y = "Term frequency (%)") + theme(legend.title = element_blank(), 
                                                       legend.position = "bottom", axis.text.x = element_text(angle = 90, 
                                                                                                              hjust = 1), axis.text = element_text(size = 8))
  }
  if (!is.null(vars_nature)) {
    res[["tf_fig_avg_prop"]] = ggplot(prop_avg_tf) + geom_bar(stat = "identity", 
                                                              aes(x = reorder(variable, tf_idf), y = tf_idf, fill = type)) + geom_text(aes(x = reorder(variable, 
                                                                                                                                                       tf_idf), y = tf_idf * 0.8, label = substr(nature_shock, 1, 3)), 
                                                                                                                                       color = "white") + scale_fill_manual(values = c("darkred", "blue", 
                                                                                                                                                                                       "darkgreen", "lightgrey", "orange", "brown")) + theme_bw() + labs(x = NULL, 
                                                                                                                                                                                                                                                         y = " % of total shock related words") + theme(legend.title = element_blank(), 
                                                                                                                                                                                                                                                                                                        legend.position = "bottom", axis.text.x = element_text(angle = 90, 
                                                                                                                                                                                                                                                                                                                                                               hjust = 1), axis.text = element_text(size = 10))
  } else {
    res[["tf_fig_avg_prop"]] = ggplot(prop_avg_tf) + geom_bar(stat = "identity", 
                                                              aes(x = reorder(variable, tf_idf), y = tf_idf, fill = nature_shock)) + 
      scale_fill_manual(values = c("darkred", "blue", "darkgreen", "lightgrey", 
                                   "orange", "brown")) + theme_bw() + labs(x = NULL, y = " % of total shock related words") + 
      theme(legend.title = element_blank(), legend.position = "bottom", 
            axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size = 10))
  }
  return(res)
}

idf_barplot = function(idf, vars_type = c("economic_shock", "non_economic_shock", 
                                          "debt_outcomes", "debt_structure",
                                          "characteristics_program", "adustment_program"), 
                       vars_nature = NULL, idf_trans = F) {
  
  #' barplot for the idf
  #' @description barplot displaying the idf of the categories
  #' @param idf a tibble of idf as produced by idf()
  #' @param vars_type a vector with the type of shocks to include (see lexicon_typology() for classification)
  #' @param vars_nature a vector of the nature of shocks to include (see lexicon_typology() for classification)
  #' @param idf_trans T use the term frequency transformation, F use the proportion
  #' @author Manuel Betin
  #' @return a ggplot barplot object 
  #' @export
  #'  

  res = list()
  
  if (idf_trans == T) {
    idf$idf = 1/exp(idf$idf)
  }
  idf = idf %>% rename(variable = Crisis)
  
  idf = idf %>% left_join(lexicon_typology(), by = c("variable"))
  
  res[["table"]] = idf
  
  if (!is.null(vars_nature)) {
    idf = idf %>% filter(type %in% vars_type & nature_shock %in% vars_nature)
  } else {
    idf = idf %>% filter(type %in% vars_type)
  }
  
  res[["fig"]] = ggplot(idf) + geom_bar(stat = "identity", aes(x = reorder(variable, 
                                                                           idf), y = idf, fill = type)) + scale_fill_manual(values = c("darkred", 
                                                                                                                                       "blue", "darkgreen", "lightgrey", "orange", "brown")) + theme_bw() + 
    lims(y = c(0, 1)) + labs(title = "Probability of the events", x = NULL, 
                             y = NULL) + theme(legend.title = element_blank(), legend.position = "bottom", 
                                               axis.text.x = element_text(angle = 90, hjust = 1), axis.text = element_text(size = 8))
  
  return(res)
  
}

