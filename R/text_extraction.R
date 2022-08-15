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

ccdr.corpus.countryupdate=function(iso3,historical_vintage,new_vintage){
  #'update corpus for a specific country
  #'@description update corpus for a specific country
  #'@param iso3 the iso3 code of the country of interest
  #'@param historical_vintage object of class corpusTM to update 
  #'@param new_vintage object of class corpusTM with the corpus of the new update 
  #'@author Manuel
  #'@export
  
  if(any(class(historical_vintage)%in%"corpusTM") & any(class(new_vintage)%in%"corpusTM") ){
    
    tryCatch({
      ctrycorpus=new_vintage[str_detect(names(mycorpus),iso3)]
      res=append(historical_vintage,ctrycorpus)
      cat(crayon::green("Succesful update, ",ctrycorpus %>% length(), "files have been included in the corpus of ", iso3,"\n"))
    },
    error=function(e){
      res=NULL
      warning(paste0("Error when updated the corpus of ",iso3,"\n"))
      warning(e)
      return(NULL)
    })  
  }else{
    warning("please provide a valid arguments for historical_vintage and new_vintage\n objects of class corpusTM are required\n")
  }
  
  return(res)
}

ccdr.transcripts.collect=function(mycorpus,keyword_list){
  #' collect transcripts identified as belonging to a specific category
  #' @description the function extract the paragraphs around the keywords that
  #' correspond to the categories of interest selected in key_word_list
  #' @param mycorpus a list of documents containing the reports of interest
  #' @param keyword_list a vector with the names of the categories to search for
  #' see ccdr.lexicon() %>% names() for details about available categories
  #' @return tibble() with the transcripts, the keyword detected, the category of
  #' crisis and the identifier of the document.
  #' @export
  #' @author Manuel Betin
  
  transcripts_all=lapply(keyword_list,function(x){
    #get transcripts for one category
    mytranscripts=ccdr.sentences(mycorpus,x)
    
    #append data for all countries
    transcripts_all=lapply(names(mytranscripts),function(y){
      mytranscripts[[y]]$doc_id=y
      mytranscripts[[y]] #%>% rename(text=sentence)
    })
    transcripts_all=do.call(rbind,transcripts_all)
    transcripts_all %>% dplyr::mutate(category=x)
  })
  #append for all categories
  transcripts_all=do.call(rbind,transcripts_all)
  return(transcripts_all)
}

ccdr.transcripts.get_nearby_words=function(word_dt,myword,n_min=10){
  #' check the frequency of nearby words
  #' @description provide a table with the frequencies of nearby words
  #' @export
  nearby_global <- word_dt %>% mutate(position = row_number()) %>%
    filter(word == myword) %>%
    select(focus_term = word, focus_position = position) %>%
    difference_inner_join(word_dt %>% tibble()  %>%
                            mutate(position = row_number()), by = c(focus_position = "position"), max_dist = 15) %>%
    mutate(distance = abs(focus_position - position))
  
  nearby_global <- nearby_global %>%
    group_by(word) %>%
    summarize(number = n(),
              maximum_distance = max(distance),
              minimum_distance = min(distance),
              average_distance = mean(distance)) %>%
    arrange(desc(number))
  
  nearby_global=nearby_global[-1,] %>% filter(number>n_min) %>% mutate(perc=number/sum(number))
  
  return(nearby_global)
  
}

ccdr.transcripts.tokenize=function(mytranscriptsdt){
  #' tokenize and analyze transcripts with tidyr
  #'@description tokenize the transcripts and remove the stop words
  #'@author Manuel Betin 
  #' @export 
  
  word_freq=mytranscriptsdt %>%
    unnest_tokens(word, sentence)%>% #tokenize
    filter(str_detect(word, "[a-z]"))%>% #keep only strings
    anti_join(stop_words) %>% #remove stop words
    mutate(word= wordStem(word),
           year=substr(doc_id,5,8),
           iso3=substr(doc_id,1,3)) %>%
    filter(year>1945)#stem words to put together close words
}

ccdr.transcripts.preprocess.Keywordsfreq=function(mytokens,myvar){
  #' tokenize and analyze transcripts with tidyr
  #'@description tokenize the transcripts and remove the stop words
  #'@author Manuel Betin 
  #'@export 
  #' 
  
  if(myvar=="Expectation"){
    mytopkeywords=mytokens %>% group_by(iso3,keyword_detected,year)%>%
      summarize(n=n())%>%arrange(-n)%>%#pull(keyword_detected)
      mutate(keywords=case_when(keyword_detected%in%c("downside risks","potential risks","upward risk","increase the risks",
                                                      "high risk","major risks","high level of risk","heightened risk aversion",
                                                      "heightening risks","increase in global risk aversion","crisis risks","risk of crisis")~"risks",
                                keyword_detected%in%c("speculative attack","self-fulfilling","speculative capital movements",
                                                      "shifts in investor sentiment")~"speculative attack",
                                keyword_detected%in%c("market confidence","bolster confidence","restore market confidence","confidence crisis",
                                                      "crisis of confidence","restoring market confidence","undermining confidence",
                                                      "bolstering market confidence","weakening of investor confidence","pressures on confidence",
                                                      "weakening of market confidence","slump in confidence","pressure on confidence","change in expectations")~"loss. confidence",
                                keyword_detected%in%c("vulnerable to abrupt swings in market sentiment","vulnerable to changes in the international investment climate","change in investors sentiment",
                                                      "shifts in investor sentiment","deterioration in market sentiment","market reversal","panic","economic sentiment remains poor")~"adv. market sentiment",
                                keyword_detected%in%c("general uncertainty","reduce market uncertainty",
                                                      "uncertainty in international capital markets",
                                                      "uncertainty among market participant","signals to markets",
                                                      "a time of heightened global uncertainty")~"uncertainty",
                                T~keyword_detected))%>%
      group_by(iso3,keywords,year)%>%
      summarize(n=sum(n))%>%arrange(-n)%>%
      mutate(year=as.numeric(year))
    
  }else if(myvar=="Sovereign_default"){
    mytopkeywords=mytokens %>% group_by(iso3,keyword_detected,year)%>%
      summarize(n=n())%>%arrange(-n)%>%#pull(keyword_detected)
      mutate(keywords=case_when(str_detect(keyword_detected,"restruct") | str_detect(keyword_detected,"exchange") | str_detect(keyword_detected,"reschedu") | str_detect(keyword_detected,"debt swap") |
                                  str_detect(keyword_detected,"reprofil")| str_detect(keyword_detected,"external creditors") |
                                  str_detect(keyword_detected,"default") | str_detect(keyword_detected,"suspension")~"default/restructuring",
                                str_detect(keyword_detected,"arrears")~"arrears",
                                str_detect(keyword_detected,"rolling over") | str_detect(keyword_detected,"roll over") | str_detect(keyword_detected,"servic") | str_detect(keyword_detected,"debt crisis") | 
                                  str_detect(keyword_detected,"fiscal crisis")  | str_detect(keyword_detected,"default risk")~"rollover",
                                str_detect(keyword_detected,"external payment") ~"external payments",
                                str_detect(keyword_detected,"debt relief") ~"debt relief",
                                T~keyword_detected))%>%
      group_by(iso3,keywords,year)%>%
      summarize(n=sum(n))%>%arrange(-n)%>%
      mutate(year=as.numeric(year))
    
  } else if(myvar=="Financial_crisis"){
    mytopkeywords=mytokens %>% group_by(iso3,keyword_detected,year)%>%
      summarize(n=n())%>%arrange(-n)%>%#pull(keyword_detected)
      mutate(keywords=case_when(str_detect(keyword_detected,"financial risk") | str_detect(keyword_detected,"volatility in financial markets") | str_detect(keyword_detected,"restore")~"Financial risk",
                                str_detect(keyword_detected,"Financial contagion")~"Financial contagion",
                                str_detect(keyword_detected,"financial crisis") | str_detect(keyword_detected,"collapse of equity prices") | str_detect(keyword_detected,"crisis in financial market") | str_detect(keyword_detected,"turmoil in financial markets") | 
                                  str_detect(keyword_detected,"unfolding financial crisis")  | str_detect(keyword_detected,"financial shock")| str_detect(keyword_detected,"global market sell-off")~"Financial crisis",
                                str_detect(keyword_detected,"global financial") | str_detect(keyword_detected,"international") ~"International financial crisis",
                                str_detect(keyword_detected,"contagion") ~"Financial contagion",
                                T~keyword_detected))%>%
      group_by(iso3,keywords,year)%>%
      summarize(n=sum(n))%>%arrange(-n)%>%
      mutate(year=as.numeric(year))
  } else if(myvar=="Fiscal_outcomes"){
    mytopkeywords=mytokens %>% group_by(iso3,keyword_detected,year)%>%
      summarize(n=n())%>%arrange(-n)%>%
      mutate(keywords=case_when(str_detect(keyword_detected,"deficit") | str_detect(keyword_detected,"financing gap")| 
                                  str_detect(keyword_detected,"borrowing needs")| str_detect(keyword_detected,"financing needs") |
                                  str_detect(keyword_detected,"imbalances") | str_detect(keyword_detected,"short-term financing need") |
                                  str_detect(keyword_detected,"fiscal position") | str_detect(keyword_detected,"new borrowing") |
                                  str_detect(keyword_detected,"budgetary") | str_detect(keyword_detected,"borrowing increased") |
                                  str_detect(keyword_detected,"public sector position deteriorate") | str_detect(keyword_detected,"budgetary gap") |
                                  str_detect(keyword_detected,"weakening of the public finance") | str_detect(keyword_detected,"pressure on public finance") |
                                  str_detect(keyword_detected,"deterioration in the fiscal situation")~"deficits",
                                str_detect(keyword_detected,"external debt") | str_detect(keyword_detected,"external financing needs") ~"external debt",
                                str_detect(keyword_detected,"domestic debt") ~"domestic debt",
                                str_detect(keyword_detected,"contigent") ~"contigent liability",
                                str_detect(keyword_detected,"stimulus") ~"policy stimulus",
                                str_detect(keyword_detected,"unsustainable") | str_detect(keyword_detected,"sustainability") | str_detect(keyword_detected,"indebtedness") |
                                  str_detect(keyword_detected,"fiscal framework")|str_detect(keyword_detected,"fiscal instability")~"unsustainable debt",
                                str_detect(keyword_detected,"expenditure") | str_detect(keyword_detected,"spending") ~"expenditure",
                                str_detect(keyword_detected,"revenue")  ~"revenue",
                                str_detect(keyword_detected,"service") | str_detect(keyword_detected,"interest payments") | str_detect(keyword_detected,"servicing") ~"debt service",
                                T~keyword_detected))%>%
      group_by(iso3,keywords,year)%>%
      summarize(n=sum(n))%>%arrange(-n)%>%
      mutate(year=as.numeric(year))
  } else if(myvar=="Inflation_crisis"){
    mytopkeywords=mytokens %>% group_by(iso3,keyword_detected,year)%>%
      summarize(n=n())%>%arrange(-n)%>%
      mutate(keywords=case_when(str_detect(keyword_detected,"pressure") | str_detect(keyword_detected,"acceler") ~"inflation pressure",
                                str_detect(keyword_detected,"hyper") | str_detect(keyword_detected,"unprecedented") ~"hyper inflation",
                                str_detect(keyword_detected,"target") | str_detect(keyword_detected,"objective") ~"inflation target",
                                str_detect(keyword_detected,"core") ~"core inflation",
                                str_detect(keyword_detected,"wage") ~"wage inflation",
                                str_detect(keyword_detected,"food") ~"food inflation",
                                str_detect(keyword_detected,"price") ~"price inflation",
                                str_detect(keyword_detected,"expect")  | str_detect(keyword_detected,"outlook")  | str_detect(keyword_detected,"future") |
                                  str_detect(keyword_detected,"forecast") | str_detect(keyword_detected,"risk")  ~"inflation expectations",
                                str_detect(keyword_detected,"lower") |str_detect(keyword_detected,"anti-inflation") |
                                  str_detect(keyword_detected,"non-inflation") |str_detect(keyword_detected,"noninflation") |
                                  str_detect(keyword_detected,"halting") |str_detect(keyword_detected,"containment") | 
                                  str_detect(keyword_detected,"combat") | str_detect(keyword_detected,"control") |
                                  str_detect(keyword_detected,"down quickly") | str_detect(keyword_detected,"curb") | str_detect(keyword_detected,"reduction")| str_detect(keyword_detected,"strategy") | str_detect(keyword_detected,"effort") ~"combat inflation",
                                str_detect(keyword_detected,"high") | str_detect(keyword_detected,"severe") | str_detect(keyword_detected,"problem") | str_detect(keyword_detected,"large") ~"high inflation",
                                T~"inflation"))%>%
      group_by(iso3,keywords,year)%>%
      summarize(n=sum(n))%>%arrange(-n)%>%
      mutate(year=as.numeric(year))
  }else{
    mytopkeywords=mytokens %>% rename(keywords=keyword_detected)%>% group_by(iso3,keywords,year)%>%
      summarize(n=n())%>%arrange(-n)%>%
      arrange(-n)%>%
      mutate(year=as.numeric(year))
  }
  
  
  return(mytopkeywords)
}

ccdr.transcripts.preprocess.ExpectationWordsfreq=function(mytokens){
  #' preprocess the category Expectations
  #'@description group similar words to make sure that the frequency of words
  #'take into account the different expressions   
  #'@author Manuel Betin 
  #'@export 
  #' 
  mydummies=mytokens %>% mutate(word=case_when(word%in%c("financi","financ","market","bank")~"financial",
                                               #word%in%c("fiscal","author","public","debt")~"fiscal intervention",
                                               word%in%c("exchang","currenc","reserv")~"currency",
                                               word%in%c("inflat","inflationari")~"inflation",
                                               word%in%c("growth","outlook","econom","macroeconom")~"growth",
                                               TRUE~word))%>%
    group_by(word)%>%summarize(n=n())%>%arrange(-n)%>%
    filter(word%in%c("risk","confid",'uncertainti','avers',"panic","distress",
                     "crisi","expect","stress","sentiment","covid",
                     "polici","growth","monetari","currency",
                     "financial","inflation","oil",
                     "commod","rate","debt","global","domest"))%>%
    arrange(-n)%>%pull(word)
  
  all_year=data.frame(1960:2022)
  colnames(all_year)="year"
  
  E_word_dummies_dt=mytokens  %>% filter(year>1945) %>% mutate(year=as.numeric(year))%>%
    right_join(all_year,by="year") %>%
    filter(word%in%mydummies) %>%
    group_by(iso3,year,word) %>% summarize(n=n())
  
  return(E_word_dummies_dt)
}

ccdr.transcripts.preprocess.SovDefaultWordsfreq=function(mytokens){
  #' preprocess the category sovereign default
  #'@description group similar words to make sure that the frequency of words
  #'take into account the different expressions   
  #'@author Manuel Betin 
  #'@export 
  #' 
  mydummies=mytokens %>% mutate(word=case_when(word%in%c("relief","reschedul","restructur","reduc","reduct","settlement",
                                                         "agreement","adjust","suspens","suspend")~"restructuring",
                                               word%in%c("program","moratorium","memorandum","moratoria")~"program",
                                               word%in%c("exclud","access","exclus","accessto")~"exclusion",
                                               word%in%c("bond","eurobond","bondhold","oblig")~"bond",
                                               word%in%c("credit","commerci","oblig")~"credit",
                                               word%in%c("foreign","extern")~"external",
                                               word%in%c("debt","liabil","borrow")~"debt",
                                               TRUE~word))%>%
    group_by(word)%>%summarize(n=n())%>%arrange(-n)%>%
    filter(word%in%c("restructuring","program","exclusion","bond","credit","external","intern",
                     "arrear","debt","govern","bank","privat","public","fiscal","deficit","hipc","liquid",
                     "revenu","tax","payment","servic","matur","rate","bilater","multilater",
                     "club","tax"))%>%
    arrange(-n)%>%pull(word)
  
  all_year=data.frame(1960:2022)
  colnames(all_year)="year"
  
  E_word_dummies_dt=mytokens  %>% filter(year>1945) %>% mutate(year=as.numeric(year))%>%
    right_join(all_year,by="year") %>%
    filter(word%in%mydummies) %>%
    group_by(iso3,year,word) %>% summarize(n=n())
  
  return(E_word_dummies_dt)
}

ccdr.transcripts.preprocess.FinCrisisWordsfreq=function(mytokens){
  #' preprocess the category financial crisis
  #'@description group similar words to make sure that the frequency of words
  #'take into account the different expressions   
  #'@author Manuel Betin 
  #'@export 
  #' 
  
  mydummies=mytokens %>% mutate(word=case_when(word%in%c("financi","market")~"financial",
                                               word%in%c("crisi","shock")~"crisis",
                                               word%in%c("global","foreign","world")~"global",
                                               word%in%c("intern","domest")~"domestic",
                                               word%in%c("exchang","currenc")~"currency",
                                               word%in%c("risk")~"risk",
                                               word%in%c("bank")~"bank",
                                               word%in%c("debt")~"debt",
                                               word%in%c("rate")~"rate",
                                               word%in%c("growth","econom","economi","macroeconom")~"economy",
                                               word%in%c("bank")~"bank",
                                               TRUE~word))%>%
    group_by(word)%>%summarize(n=n())%>%arrange(-n)%>%
    filter(word%in%c("financial","crisis","global","domestic","currency","risk","bank",
                     "debt","rate","economy","bank"))%>%
    arrange(-n)%>%pull(word)
  
  all_year=data.frame(1960:2022)
  colnames(all_year)="year"
  
  E_word_dummies_dt=mytokens  %>% filter(year>1945) %>% mutate(year=as.numeric(year))%>%
    right_join(all_year,by="year") %>%
    filter(word%in%mydummies) %>%
    group_by(iso3,year,word) %>% summarize(n=n())
  
  return(E_word_dummies_dt)
}

ccdr.transcripts.plot.KeywordsFreq=function(mytopkeywords,mywords){
  #' plot the keyword frequency
  #'@description 
  #'@author Manuel Betin 
  #'@export 
  #' 
  
  res=mytopkeywords%>%
    filter(year>1990)%>%
    left_join(incomegroup,by=c("iso3"))%>%
    group_by(year,keywords,income_group)%>%filter(year>1960)%>%
    summarize(n=sum(n,na.rm=T))%>%
    filter(keywords%in%mywords) %>%
    group_by(income_group,keywords)%>%
    mutate(n.norm=n/max(n))
  
  fig=res%>%
    ggplot(aes(x=as.numeric(year),y=n.norm,group=keywords,fill=keywords,color=keywords,linetype=keywords))+
    geom_rect(aes(ymin=-Inf,ymax=Inf,xmin=2020,xmax=2022,),
              fill="lightgrey",color="lightgrey",alpha=0.05,
              show.legend = F)+
    geom_point(stat="identity",size=0.5)+
    geom_line(stat="identity")+
    scale_x_continuous(breaks=c(seq(1990,2020,5),2022))+
    facet_wrap(~income_group,scale="free_x")+
    theme_ipsum()+
    labs(x=NULL,y="norm tf")+
    theme(legend.title = element_blank(),
          axis.text.x=element_text(angle=90),
          legend.position = "bottom",
          panel.grid.major.x  = element_line(color="#c8c8c8", size = 0.2))
  fig
  return(fig)
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





