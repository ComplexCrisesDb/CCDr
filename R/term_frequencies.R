ccdr.tf <- function(corpus, keywords, brute_freq = F, parrallel = T) {
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
    table <- lapply(corpus, function(x) {

      # Exclude minutes meetings, press releases and working papers erroneously scraped.
      # Check the first page of each document for bad keyword.

      valid_document <- tibble(first_page = tolower(x[[1]])) %>%
        filter(!str_detect(first_page, paste(ccdr.lexicon()[["Problematic_documents"]], collapse = "|"))) %>%
        nrow()

      # If is a valid document, continue with tf-calculation. Else, return NA.

      if (valid_document == 1) {
        output <- try(sum(ccdr.pages(x, keywords,
          brute_freq = brute_freq,
          parrallel = parrallel
        )[, 1]))
        if ("try-error" %in% class(output)) {
          cat(crayon::red(paste0("Warning: error when mining ", keywords)))
          output <- 0
        }
        output
      }
      else {
        NA
      }
    })
    names(table) <- names(corpus)
    table <- do.call(rbind, table)
    table <- dplyr::tibble(table)
    table$file <- names(corpus)
    names(table) <- c("var", "file")
    return(table)
  } else {
    cat(crayon::red("please provide a vector of strings as argument for keywords"))
  }
}

ccdr.tfs <- function(corpus, lexicon, brute_freq = F, parrallel = T, centre_countries = "USA") {
  #' vectorize the function tf() to be able to pass a list of names of keywords
  #' keyword_list is a list containing the names of different groups of
  #' keywords that have a vector of words to look into.
  
  #' @param corpus a list of texts from pdf_text() with different names
  #' for each element
  #' @param lexicon a lexicon in the form of a list containing a vector of keywords, 
  #' the list is named with the name of the category of interest
  #' @param brute_freq T/F if T it will just count the occurence, otherwise
  #'  it will compute the term frequency
  #' @param parrallel T/F if T it will use mclapply from the parrallel package
  #' @param centre_countries Character string. Default is "USA". Netting of confusing
  #' keywords will not be performed countries in the vector.
  #' @author Manuel Betin
  #' @return a tibble with the term frequencies for the selected categories
  #' @export
  
  #remove confusing categories
  rm_confusing=str_detect(names(lexicon),"_confusing")
  lexicon=lexicon[!rm_confusing]
  
  progress <- dplyr::progress_estimated(length(names(lexicon)))
  list_table_keyword_occurence <- lapply(1:length(names(lexicon)), function(x) {
    cat(crayon::bgBlack("\n"))
    cat(crayon::green(paste0(
      "(", x, "/", length(names(lexicon)), ") running: ",
      names(lexicon)[x], "\n"
    )))
    tictoc::tic(names(lexicon)[x])
    if (!"character" %in% class(lexicon[[x]])) {
      warning("please provide a valid vector of characters")
      dt <- NULL
      tictoc::toc()
      dt
    } else {
      res <- try({
        dt <- ccdr.tf(corpus, lexicon[[x]],
                      brute_freq = brute_freq,
                      parrallel = parrallel
        )
        dt <- dt %>% dplyr::rename(`:=`(
          !!paste0(names(lexicon)[x]),
          var
        ))
        dt
      })
      if ("try-error" %in% class(dt)) {
        cat(crayon::red(paste0(
          "\n Error: term frequency as not been computed",
          "\n"
        )))
        res <- NULL
      }
      tictoc::toc()
      progress$pause(0.01)$tick()$print()
      cat(crayon::green(paste0(
        "\n Finished running: ", names(lexicon)[x],
        "\n"
      )))
      res
    }
  })
  names(list_table_keyword_occurence) <- names(lexicon)
  
  
  dt <- list_table_keyword_occurence[[1]]
  
  # If more than one index, reduce the merge.
  
  if (length(names(lexicon)) > 1) {
    for (i in 2:length(list_table_keyword_occurence)) {
      res <- try({
        dt <- dt %>% dplyr::left_join(list_table_keyword_occurence[[i]],
                                      by = c("file")
        ) #' ISO3_Code','Period','type_prog','year',
        dt
      })
      if ("try-error" %in% class(res)) {
        dt <- dt
      } else {
        dt <- res
        dt <- dt %>%
          dplyr::select(file, everything()) %>%
          dplyr::distinct() # ISO3_Code,Period,year,type_prog,
      }
    }
  }
  
  # At this stage dataframe with file name and all different indexes.
  # Include additional step to net out index computed on confusing keywords (e.g. referring to other countries):
  
  # List of "confusing" categories on which the index could be computed:
  
  list_net_keywords <- str_extract(names(ccdr.lexicon()), ".+_confusing")[complete.cases(str_extract(names(ccdr.lexicon()), ".+_confusing"))]
  
  # Double check: check if we calculated some and check if country different from centre countries. If TRUE,
  # proceed to net. Otherwise return df.
  tryCatch({
    if (any(list_net_keywords %in% names(dt)) & unique(str_extract(dt$file, "[A-Z]{3}") != centre_countries)) {
      dt <- split.default(dt, str_remove(names(dt), "_.+")) # split into list according to the first word of column name, removing the rest.
      
      dt <- dt %>%
        purrr::map(~ if (any(names(.x) != "file")) {
          if (any(str_detect(names(.x), "confusing"))) { # if column name with "confusing" within same category
            colnames(.x)[grepl("confusing", colnames(.x))] <- "confusing" # subtract from other indexes.
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
    
      dt %>% # Bind net indexes with file list.
        purrr::reduce(cbind) %>%
        select(file, everything())
    } else {
      dt
    }
  },
  error=function(e){
    cat(crayon::red("index could not be net from confusing lexicon, raw indexes are returned by default\n"))
    cat(crayon::red(e))
    return(dt)
  })
 
}

run.ccdr.tfs <- function(corpus_file,lexicon,export_path = NULL, parrallel = T) {
  
  #' Compute the tf matrix for a corpus given in a list format,
  #' @description the type of lexicon to use and the sublist of keywords associated The
  #' output is a matrix of tf with a row per document and a column for each
  #' element of the keyword list
  #' @param corpus_file the path to the RData file containing the corpus to analyze
  #' @param lexicon a lexicon in the form of a list containing a vector of keywords, 
  #' the list is named with the name of the category of interest
  #' @param export_path the path were to export the tf
  #' @param parrallel T/F to use mclapply from the parrallel package
  #' @return a dataframe of term frequencies
  #' @author Manuel Betin
  #' @export
  
  
  
  cat(crayon::bgBlue(paste0("Loading corpus from ", corpus_file)))
  corpus <- rio::import(corpus_file)
  if (is.null(lexicon)) {
    print("please return a valid lexicon")
    return(NA)
  }
  
  tictoc::tic()
  dt <- ccdr.tfs(corpus, lexicon, parrallel = parrallel)
  tictoc::toc()
  destination <- paste0(export_path, "/ccd_tf.RData")
  print(paste0("export table in ", corpus_file))
  if (!is.null(export_path)) {
    rio::export(dt, destination)
  }
  return(dt)
}

ccdr.tfs.update <- function(file_tf_to_update,corpus_file,lexicon = NULL,export_file,
                            parrallel = T, store_old = F, store_old_path = NULL) {
  #' Updates the tf-indexes without repeating full extraction
  #'
  #' @description Combine probability of shocks, intensity and complexity of relations to construct
  #' .a mesure of severity of crisis
  #'
  #' @param file_tf_to_update Path to old tf dataframe.
  #' @param corpus_file Path to corpus from which perform the extraction.
  #' @param lexicon new lexicon of categories
  #' @param export_file Path to export the file.
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
  
  if (is.null(lexicon)) {
    print("please provide a valid lexicon")
    return(NA)
  } else {
    cat(crayon::green("updating selected columns"))
    tf_to_update <- rio::import(file_tf_to_update)
    dim_tf_to_update <- dim(tf_to_update)
    existing_cols <- names(tf_to_update)
    
    if (any(existing_cols %in% names(lexicon))) {
      # Remove only columns in both update list and old dataframe:
      existing_keyword_list <- intersect(existing_cols, names(lexicon))
      tf_to_update <- tf_to_update %>% dplyr::select(-existing_keyword_list)
    }
    
    corpus <- rio::import(corpus_file)
    
    new_tf <- run.ccdr.tfs(
      corpus_file = corpus_file, 
      lexicon = lexicon, parrallel = parrallel
    )
    
    tf_to_update <- dplyr::left_join(x = tf_to_update, y = new_tf, by = "file")
    
    print(paste0("Non updated columns:\n
                 ", paste0(existing_cols,
                           collapse = ", "
    )))
    
    print(paste0("Updated columns:\n
                 ", paste0(names(lexicon),
                           collapse = ", "
    )))
    
    if (store_old == T) {
      if (dir.exists(store_old_path) != T) {
        dir.create(store_old_path)
      }
      # Move old files:
      file.move(file_tf_to_update, store_old_path, overwrite = TRUE)
    }
    
    rio::export(tf_to_update, export_file)
    
    return(tf_to_update)
  }
}

scrap.ccdr.tfs <- function(urls = url_links, lexicon, extract_number = 1,
                           ENGINE = pdf_text,delete_pdfs = T,rm_short_docs = F,
                           min_words = 100,parrallel = T, loc_temp = NULL) {
  
  #' run the term frequency matrix on the list of urls provided as parameter.
  #' @description The function download the pdf, create the corpus and generate the term
  #' frequency matrix, to avoid storage limitation the function deleate
  #' original pdf files
  
  #' @param urls a dataframe containing two columns: name_file and pdf
  #' that respectively provid the name that will be given to the file
  #' downloaded and pdf that contains the url of the pdf
  #' @param lexicon one of the element of the list provided by
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
  if (is.null(loc_temp)) {
    path <- "temp"
  } else {
    path <- loc_temp
  }
  
  path_pdf_files <- paste0(path, "/files")
  path_corpus <- paste0(path, "/corpus")
  path_tf <- paste0(path, "/tf")
  
  dir.create(path)
  dir.create(path_pdf_files)
  dir.create(path_corpus)
  dir.create(path_tf)
  
  # download the files
  scrap.ccdr.files(urls, path_pdf_files, overwrite = F)
  # transform pdf to character and store in list
  corpus <- ccdr.corpus(path_pdf_files, ENGINE = ENGINE, only_files = T)
  
  # remove documents with less than specified number of words
  if (rm_short_docs) {
    cat(crayon::blue(paste0("\n Remove from corpus the documents with less than ", min_words, " words\n")))
    N_char_corpus <- sapply(1:length(corpus), function(x) {
      sum(stri_count_words(corpus[[x]]))
    })
    names(N_char_corpus) <- names(corpus)
    N_char_corpus <- data.frame(N_char_corpus)
    N_char_corpus$file <- names(corpus)
    N_char_corpus <- N_char_corpus %>% filter(N_char_corpus >= min_words)
    corpus <- corpus[N_char_corpus$file]
  }
  
  # save corpus
  save(corpus, file = paste0(
    path_corpus, "/corpus_", extract_number,
    ".RData"
  ))
  
  # delete folder with pdf after consolidating
  cat(crayon::blue("delete folder with pdf \n"))
  if (delete_pdfs) {
    unlink(path_pdf_files, recursive = T)
  }
  # run the term frequency matrix
  dt <- run.ccdr.tfs(
    corpus_file = paste0(
      path_corpus, "/corpus_",
      extract_number, ".RData"
    ), lexicon = lexicon,
    export_path = path_tf, parrallel = parrallel
  )
  
  file.rename(
    from = paste0(path_tf, "/ccd_tf.RData"),
    to = paste0(
      path_tf, "/ccd_tf_", extract_number,
      ".RData"
    )
  )
}

# append old and new vintages of term frequencies
ccdr.tfs.updateyears=function(old_vintage,updated_years){
  #' append old vintage and new vintage updated for new data
  #' @description update the old vintage of the database with the the new dataset
  #' for the updated years
  #' @param old_vintage data.frame of the old vintage of the database for which new years are available
  #' @param updated_years data.frame of the new variables updated.
  #' @author Manuel Betin
  #' @export
  
  if(!(any(names(old_vintage)%in%c("iso3")) & any(names(old_vintage)%in%c("year")))){
    cat(crayon::red("Please provide a valid historical vintage, identifiers 'year' and 'iso3' are necessary"))
    return(NULL)
  }
  myoldvars=setdiff(names(old_vintage),c("iso3","year"))
  
  mynewvars=setdiff(names(updated_years),c("iso3","year"))
  
  vars_inold_notinnew=setdiff(myoldvars,mynewvars)
  if(length(vars_inold_notinnew)>0){
    cat(crayon::blue(length(vars_inold_notinnew),"variable of the old vintage is not available in new vintage :\n" , paste0("- ",vars_inold_notinnew),'\n',collapse=" "),"\n")
  }
  
  vars_innew_notinold=setdiff(mynewvars,myoldvars)
  if(length(vars_innew_notinold)>0){
    cat(crayon::blue(length(vars_innew_notinold),"variable of the new vintage is not available in the old vintage :\n" , paste0("- ",vars_innew_notinold),'\n',collapse=" "),"\n")
  }
  varsready=myoldvars[myoldvars%in%mynewvars]
  cat(crayon::green(length(varsready),"variables ready for update \n",paste0("- ",varsready,'\n',collapse=" ")))
  
  old_vintage=old_vintage%>%mutate(vintage_status="old")
  updated_years=updated_years%>%mutate(vintage_status=paste0("updated ",Sys.Date()))
  
  old_vintage=old_vintage%>%dplyr::select("iso3","year",varsready,"vintage_status")
  updated_years=updated_years %>%dplyr::select("iso3","year",varsready,"vintage_status")
  
  tryCatch({res=rbind(old_vintage,updated_years)
  res %>% tibble()},
  error=function(e){
    warning("Unable to merge the two dataset\n")
    warning(e)
    res=NULL
    return(res)
  })
  res %>% group_by(year)%>%summarize(n=n())%>%tail()%>%print()
  return(res)
}

# Transformations on term frequencies
ccdr.tfs.normalize=function(mydata,var2normalize){
  #' nomarlize term frequencies
  #'@description normalize the variable of interest by removing the
  #' country specific means and dividing by the standard deviations
  #' when all serie is zero then the value is set to zero.
  #' @param mydata a vintage of the CCDB dataset
  #' @param var2normalize a vector of column names for which the normalisation is
  #' performed.
  #' @author Manuel Betin
  #' @export
  #' 
  for(var in var2normalize){
    crisis_threshold=mydata %>% filter(get(var)>0) %>%group_by(iso3) %>%
      dplyr::select(iso3,var) %>%
      summarize(!!paste0(var,"_t",0.05):=quantile(get(var),0.05,na.rm=T)[[1]],
                !!paste0(var,"_t",0.25):=quantile(get(var),0.25,na.rm=T)[[1]],
                !!paste0(var,"_t",0.5):=quantile(get(var),0.5,na.rm=T)[[1]],
                !!paste0(var,"_t",0.8):=quantile(get(var),0.8,na.rm=T)[[1]],
                !!paste0(var,"_mean"):=mean(get(var),na.rm=T)[[1]],
                !!paste0(var,"_sd"):=sd(get(var),na.rm=T)[[1]],
                !!paste0(var,"_sd"):=ifelse(is.na(paste0(var,"_sd")),0,get(paste0(var,"_sd"))))
    
    mydata=mydata %>%
      left_join(crisis_threshold,by="iso3") %>%
      mutate(!!paste0(var,"_p",0.05):=ifelse(get(var)>get(paste0(var,"_t",0.05)),get(var),0),
             !!paste0(var,"_p",0.25):=ifelse(get(var)>get(paste0(var,"_t",0.25)),get(var),0),
             !!paste0(var,"_p",0.5):=ifelse(get(var)>get(paste0(var,"_t",0.5)),get(var),0),
             !!paste0(var,"_p",0.8):=ifelse(get(var)>get(paste0(var,"_t",0.8)),get(var),0),
             !!paste0(var,"_norm"):=ifelse(!is.na(get(paste0(var,'_sd'))),(get(var)-get(paste0(var,'_mean')))/get(paste0(var,'_sd')),0))
    
  }
  return(mydata)
}

ccdr.tfs.typologycrisis=function(mydata){
  #' compute typology of crisis
  #' @description compute the typology of crisis in the spirit of Betin 2022
  #' that discriminate FGS and SGS crisis
  #' @param mydata a vintage of the CCDB database
  #' @author Manuel Betin
  #' @export
  
  mydata=mydata %>% mutate(ComplexCrises=ifelse(Sovereign_default_p0.25>0 &
                                                  Expectations_p0.25>0 #& (polici>0 |fiscal>0 |monetari>0)
                                                ,1,0),
                           SimpleCrises=ifelse(Sovereign_default_p0.25>0 &
                                                 Expectations_p0.25==0 #&(polici==0 |fiscal==0 |monetari==0)#&
                                               ,1,0),
                           SimpleRecession=ifelse(Severe_recession>0 &
                                                    Sovereign_default_p0.25==0 &
                                                    Expectations_p0.25==0 #& (polici==0 |fiscal==0 |monetari==0)#&
                                                  ,1,0),
                           NormalTimes=ifelse(Severe_recession==0 &
                                                Expectations_p0.25==0 &# (polici==0 |fiscal==0 |monetari==0) &
                                                Sovereign_default_p0.25==0 &
                                                Soft_recession_p0.25==0
                                              ,1,0),
                           Expansion=ifelse(Expansion_p0.25>0 &# (polici==0 |fiscal==0 |monetari==0)&
                                              NormalTimes==0 &
                                              SimpleRecession==0 &
                                              SimpleCrises==0 &
                                              ComplexCrises==0
                                            ,1,0)) %>%
    mutate(`Crises`=ifelse(ComplexCrises==1,"Complex",
                           ifelse(SimpleCrises==1 & ComplexCrises==0,"Simple",
                                  ifelse(SimpleCrises==0 & ComplexCrises==0 & SimpleRecession==1,"Recession",
                                         ifelse(Expansion==1 & ComplexCrises==0 & SimpleCrises==0 & SimpleRecession==0,"Expansion","Normal"))))) %>%
    mutate(SimpleCrises=ifelse(Crises=="Simple",1,0),
           ComplexCrises=ifelse(Crises=="Complex",1,0),
           SimpleRecession=ifelse(Crises=="Recession",1,0),
           Expansion=ifelse(Crises=="Expansion",1,0)) %>%
    mutate(Institutional_crisis=ifelse(Political_crisis_p0.25>0|Social_crisis_p0.25>0,1,0))
  return(mydata)
}


## Figures 


ccdr.tfs.fig.radiography=function (CCDB_y, classif = "all", vars, years_crisis, window,my_var_to_highligh=NULL) {
  #' plor the variations of indices around crisis by income group
  #' @description plot the selected indicators in a window around crisis period for the 
  #' selected time span and the income group of interest
  #' @param CCDB_y a vintage of the CCDB dabaset
  #' @param classif all/income_group/region
  #' @param vars a vector with the names of the categories of interest
  #' @param years_crisis a vector of dates for the crisis of interest
  #' @param window a vector with the range of periods around the crisis event
  #' @author Manuel Betin
  #' @export
  #' 
  ncol=3
  events = lapply(years_crisis, function(x) {
    window_vec = (x + min(window)):(x + max(window))
    CCDB_y %>% filter(year %in% window_vec) %>% 
      dplyr::select(iso3,income_group,region, year, vars) %>% 
      mutate(h = as.numeric(year) - x) %>%
      gather(key = "variable", value = "value",-c(iso3, income_group,region,year, h)) %>%
      mutate(crisis = as.character(x))
  })
  res = do.call(rbind, events)%>%
    mutate(variable=str_replace_all(variable,"_"," "),
           variable=str_replace_all(variable,"severe",""),
           variable=str_replace_all(variable,"crisis",""))
  
  if(classif=="all"){
    res = res %>% group_by(year,crisis,h,variable)%>%
      summarize(value=mean(value,na.rm=T))%>%
      mutate(myalpha=ifelse(crisis==2020,1,0),
             mylinetype=ifelse(crisis==2020,"1","0"))
    
    
  }else if(classif=="High income"){
    res = res %>% filter(income_group==classif)%>%
      group_by(year,crisis,h,variable)%>%
      summarize(value=mean(value,na.rm=T))%>%
      mutate(myalpha=ifelse(crisis==2020,1,0),
             mylinetype=ifelse(crisis==2020,"1","0"))
  }else if(classif=="Low income"){
    res = res %>% filter(income_group==classif)%>%
      group_by(year,crisis,h,variable)%>%
      summarize(value=mean(value,na.rm=T))%>%
      mutate(myalpha=ifelse(crisis==2020,1,0),
             mylinetype=ifelse(crisis==2020,"1","0"))
  }else if(classif=="Upper middle income"){
    res = res %>% filter(income_group==classif)%>%
      group_by(year,crisis,h,variable)%>%
      summarize(value=mean(value,na.rm=T))%>%
      mutate(myalpha=ifelse(crisis==2020,1,0),
             mylinetype=ifelse(crisis==2020,"1","0"))
  }else if(classif=="Lower middle income"){
    res = res %>% filter(income_group==classif)%>%
      group_by(year,crisis,h,variable)%>%
      summarize(value=mean(value,na.rm=T))%>%
      mutate(myalpha=ifelse(crisis==2020,1,0),
             mylinetype=ifelse(crisis==2020,"1","0"))
  }
  
  
  fig=res%>%mutate(value=value*1000)%>%
    ggplot() +
    geom_point(aes(x = h, y = value,color = crisis, group = crisis,alpha=myalpha)) +
    geom_line(aes(x = h,y = value, color = crisis, group = crisis,alpha=myalpha,linetype=mylinetype)) +
    geom_rect(data = subset(res, variable %in% my_var_to_highligh), 
              fill = NA, colour = "red", xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf,size=1.5)+
    facet_wrap(~variable,scale = "free", ncol = ncol) +
    scale_x_continuous(breaks = min(window):max(window)) + 
    scale_alpha(guide = 'none',range=c(1,1))+
    scale_color_manual(values=c("grey","black"))+
    theme_ipsum() +
    guides(linetype="none")+
    theme(legend.position = "bottom",
          legend.title = element_blank()) + 
    labs(y = "Term freq. (percent)", x = NULL)  
  
  return(fig)
}



ccdr.tfs.fig.tsfreq=function(CCDB_y,myvar,type="perc", year_window = c(1950,2022)){
  #' plot the time serie of the index for the selected statistic
  #' @description plot the total number of countries (N), the percentage (perc),
  #' or the average term frequency (mean) for the select indicators by income 
  #' group.
  #' @param CCDB_y a vintage of the CCDB dabaset
  #' @param myvars the names of the category of interest
  #' @param type N/mean/perc
  #' @param year_window a vector with the range of periods around the crisis event
  #' @author Manuel Betin
  #' @export
  #' 
  res=CCDB_y %>% 
    filter(!is.na(income_group))%>%
    filter(year %in% min(year_window):max(year_window)) %>% 
    group_by(income_group,year) %>% 
    summarize(n=sum(ifelse(get(myvar)>0,1,0),na.rm=T),
              tot=sum(ifelse(!is.na(get(myvar)),1,0),na.rm=T),
              mean=mean(get(myvar),na.rm=T))%>%
    mutate(perc=n/tot)
  
  if(type=="perc"){
    res = res %>% ggplot(aes(x=as.numeric(year),y=perc))+
      lims(y=c(0,1))
  }else if(type=="N"){
    res = res %>% ggplot(aes(x=as.numeric(year),y=n))
  }else if(type == "mean"){
    res = res %>%  ggplot(aes(x=as.numeric(year),y=mean))
  }
  
  res=res+
    geom_bar(stat="identity",color="white")+
    scale_x_continuous(breaks=seq(CCDB_y$year%>%min()%>%as.numeric(),CCDB_y$year%>%max()%>%as.numeric(),5))+
    labs(x=NULL,
         y=myvar)+
    theme_ipsum() +
    scale_fill_viridis_d()+
    facet_wrap(~income_group)+
    theme(legend.title = element_blank(),
          axis.text.x=element_text(angle=90),
          legend.position = "top",
          panel.grid.major.x  = element_line(color="#c8c8c8", size = 0.2))
  return(res)
}

ccdr.tfs.fig.ctrycoverage=function(CCDB_y,type_classif="income_group",year_window = c(1950,2022),const_sample_since=2022){
  #' plot the number of countries in the sample by year
  #' @description time series with the number of country in the sample by income_group or region
  #' according to the world bank classification
  #' @param CCDB_y a vintage of the CCDB dataset
  #' @param type_classif the type of country classification for the grouping variable
  #' @param year_window a vector with the first and last year of interest
  #' @param const_sample_since the date before which only members countries are available in 
  #' the sample
  #' @author Manuel Betin
  #' @export
  
  myisobef1960=CCDB_y%>%filter(year<const_sample_since)%>%pull(iso3)%>%unique()
  
  CCDB_y %>% filter(iso3%in%myisobef1960)%>% group_by(get(type_classif),year,vintage_status)%>%filter(between(year,year_window[1],year_window[2]))%>%
    summarize(ccdb.ctries=n())%>%
    rename(!!type_classif:=`get(type_classif)`)%>%
    filter(!is.na(get(type_classif)))%>%
    ggplot()+
    geom_bar(stat="identity",aes(x=year,y=ccdb.ctries,fill=get(type_classif)),color="white")+
    theme_ipsum()+
    labs(x=NULL)+
    theme(legend.position = 'bottom',
          legend.title = element_blank())
}

ccdr.tfs.fig.eventstudy=function(CCDB_y,iso3c,myvar){
  #' plot the crisis index for the selected country
  #' @description plot the selected indicators for the country of interest
  #' @param CCDB_y a vintage of the CCDB dabaset
  #' @param iso3c the iso3 code of the country of interest
  #' @param myvar the name of the indicator to plot
  #' @author Manuel Betin
  #' @export
  CCDB_y %>% filter(iso3==iso3c) %>% 
    mutate(year=as.numeric(year))%>%
    ggplot(aes(x=year,group=iso3))+
    geom_line(aes(y=get(myvar)),color="red")+
    scale_x_continuous(breaks=seq(CCDB_y$year%>%min()%>%as.numeric(),CCDB_y$year%>%max()%>%as.numeric(),5))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
    labs(x=NULL,
         y="Term frequency")+
    theme_ipsum() +
    theme(legend.title = element_blank(),
          axis.text.x=element_text(angle=90),
          legend.position = "top",
          panel.grid.major.x  = element_line(color="#c8c8c8", size = 0.2)
    )
  
}


ccdr.tfs.fig.network=function(CCDB_y,min_year=2018,max_year=2022,min_corr=0.2){
  
  #' plot the network of crisis
  #' @description plot the network of the correlations matrix of crisis for the 
  #' specific window of periods. 
  #' @param CCDB_y a vintage of the CCDB dataset
  #' @param min_year the first year of the window
  #' @param max_year the last year of the window
  #' @param min_corr the minimum correlation to draw the edges in the network
  #' @author Manuel Betin
  #' @export
  #' 
  #function that set the correlation to 0 under a specific threshold
  degree_threshold = function(x) {
    ifelse(x > min_corr, x, 0)
  }
  
  set.seed(12345)
  # create correlation matrix
  corr_matrix = CCDB_y %>% 
    ungroup()%>% 
    filter(year > min_year & year < max_year) %>% 
    dplyr::select(myshocks) %>% 
    cor() %>% 
    data.frame() %>% 
    mutate_all(degree_threshold)
  
  # adjust matrix to network format
  myadjmatrix=graph_from_adjacency_matrix(as.matrix(corr_matrix), mode = "undirected", diag = F, weighted = T)
  
  mynetworkdata=toVisNetworkData(myadjmatrix)
  
  # Set the parameters for the visualisation of the network
  
  mynodes=mynetworkdata$nodes
  mynodes=mynodes%>%mutate(size=25,
                           font.size=35,
                           color="lightgrey",
                           shadow=T,
                           value=10,
                           shape='circle')
  
  mynodes$label=c("Sov.","Bank","Fin.","Wars","Pol.","Nat.","Mig.","Infl.",
                  "Cont.","Com.","World","Curr.","Exp.","Rec.","Bop","Soc.",
                  "Epid.","Hous.")
  
  myedges=mynetworkdata$edges%>%
    mutate(color = case_when(weight >= 0.5 ~ "darkred",
                             weight < 0.5 & weight >= 0.3 ~ "darkorange",
                             TRUE ~ "gold")) %>%
    mutate(width = case_when(weight >= 0.5 ~ 10,
                             weight < 0.5 & weight >= 0.3 ~ 8,
                             TRUE ~ 3))%>%
    mutate(lty=case_when(weight >= 0.5 ~ 0,
                         weight < 0.5 & weight >= 0.3 ~ 1,
                         TRUE ~ 2))
  
  # plot the network
  
  mynetwork=visNetwork(nodes = mynodes,
                       edges=myedges,
                       ledges = data.frame(color = c("darkred","darkorange","gold"), label = c("> 0.4","0.4 - 0.2","< 0.2"), arrows = c("undirected"),
                                           width = c(6,3,1), font.align = "top"))%>%
    visNodes(color = list(background = "gray", border = "black")) %>% 
    visPhysics(solver = "forceAtlas2Based",forceAtlas2Based = list(gravitationalConstant = -100)) %>%
    visLegend(addEdges = mynetworkdata$ledges, position = "right") %>% 
    visLayout(randomSeed = 346)%>%
    visInteraction(dragNodes = FALSE, 
                   dragView = FALSE, 
                   zoomView = T) 
  return(mynetwork)
}


ccdr.tfs.fig.eventstudyYES=function(CCDB_y,iso3c){
  
  #' plot the number of in crisis by typology
  #' @description plot the time series of SGS,FGS and risk free crisis for the 
  #' selected country
  #'  SGS crisis corresponds to sovereign crisis with expectation regime
  #' shift, FGS to sovereign crisis without expectation regime shifts and risk free 
  #' recessions crisis without sovereign debt concerns.
  #' 
  #' @param CCDB_y a vintage of the CCDB dataset
  #' @param iso3c the iso3 code of the country of interest
  #' @author Manuel Betin
  #' @export
  #' 
  #' 
  CCDB_y %>% filter(iso3==iso3c) %>% 
    mutate(year=as.numeric(year))%>%
    ggplot(aes(x=year,group=iso3))+
    geom_line(aes(y=Sovereign_default_p0.25,color="Sovereign (S)"))+
    geom_line(aes(y=Expectations_p0.25,color="Expectations (E)"))+
    geom_line(aes(y=Severe_recession_p0.25,color="Recession (Y)"))+
    scale_color_manual(values=c("orange","red","darkgreen"))+
    scale_x_continuous(breaks=seq(CCDB_y$year%>%min()%>%as.numeric(),CCDB_y$year%>%max()%>%as.numeric(),5))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))+
    labs(x=NULL,
         y="Term frequency")+
    theme_ipsum() +
    theme(legend.title = element_blank(),
          axis.text.x=element_text(angle=90),
          legend.position = "top",
          panel.grid.major.x  = element_line(color="#c8c8c8", size = 0.2)
    )
  
}



