ccdr.transcripts.compile=function(myvar,ctries,path_corpus,path_export){
  #' Get transcripts for a specific variable
  #' @description use the CCDB dictionary to extract and label each sentence of 
  #' the country reports.
  #' @param myvar the name of the category in the dictionary that will be extracted
  #' @param ctries a vector of countries of which extract the transcripts
  #' @param path_corpus the path of the directory where the corpus for each country
  #' are located 
  #' @param path_export the path of the directory where to save the specific transcripts
  #' @author Manuel Betin
  #' @export
  
  mytranscripts=lapply(ctries,function(x){
    #path to the country specific corpus
    mycorpus_path=paste0(path_corpus,x)
    print(paste0(x,"\n"))
    #import corpus
    tryCatch({
      mycorpus=rio::import(mycorpus_path)
      # Collect the transcripts where references to specific crisis appear
      transcripts_all=ccdr.transcripts.collect(mycorpus,myvar)
      transcripts_all
    },
    error=function(e){
      warning(e)
      NULL
    })
  })
  mytranscriptsdt=do.call(rbind,mytranscripts)
  
  # Export transcripts
  rio::export(mytranscriptsdt,
              paste0(path_export,"ccdb_",myvar,"_transcripts.csv"))
  
  return(mytranscriptsdt)
}



ccdr.transcripts.fasttextsample=function(path_transcripts,training_size=0.7,min_char=30,min_word=15,sentiment_threshold=5,neutral_sentiment=c(-0.25,0.25),
                                         multi_label=T,
                                         rm_stop_words=T,
                                         sentiment_corpus=NULL){
  #' aggregate the transcript into training and test sample
  #' @description create a training and test sample of text compatible with the 
  #' fasttext algorithm, for the supervized  text recognition model
  #' @author Manuel Betin
  #' @param path_transcripts a vector containing the path to the ccdb_transcripts
  #' (output of the function ccdr.transcripts.collect())
  #' @param training_size proportion of the sample allocated to training sample
  #' @param min_char Threshold for minimum number of character for sentences to be valid
  #' @param min_word threshold for minimum number of words for sentences to be valid
  #' @param sentiment_threshold only keep sentences with negative sentiment below the threshold.
  #' @param neutral_sentiment vector with the bounds for the definition of neutral sentiment
  #' @param multi_label allow several labels per sentences.  
  #' @param rm_stop_words T/F remove stopwords 
  #' @param sentiment_corpus a dataset with the classification of negative and positive words, the dataset should 
  #' have at least two colonnes word and sentiment. 
  #' @export
  #' 
  
  
  fasttextsample=lapply(files,function(x){
    dt=rio::import(x)
    
    mycategory=dt%>%pull(category)%>%unique()
    
    dt=dt %>%
      mutate(sentence=as.character(sentence))%>%
      mutate(n.char=nchar(sentence),
             n.word=str_count(sentence, '\\w+'))%>%
      filter(n.char>min_char,
             n.word>min_word)
    
    
    dt$sentence.id=1:dim(dt)[1]    
    # remove numbers from the sentence
    dt$sentence=str_replace_all(dt$sentence,"\\d","")
    # remove punctuation from the sentences
    dt$sentence=str_replace_all(dt$sentence, "[[:punct:]]", "") 
    
    dt$sentence=str_squish(dt$sentence)
    
    # remove non alpha numeric characters
    #dt$sentence=str_replace_all(dt$sentence, "[^[:alnum:]]", "")    # Delete non-alphanumeric
    
    dt=dt%>%mutate(sentence=paste0("__label__",gsub("_","",category),", ",sentence))%>%
      dplyr::select(sentence,sentence.id)
    # remove sentences that have less than 15 words and 30 characters
    dt=dt%>%
      mutate(n.char=nchar(sentence),
             n.word=str_count(sentence, '\\w+'))%>%
      filter(n.char>30,
             n.word>10)%>%
      dplyr::select(sentence.id,sentence)
    
    #remove stopwords
    if(rm_stop_words){
      dt=dt%>%
        tidytext::unnest_tokens(words,sentence,token="words")%>%
        filter(!words %in% stop_words$word)%>%
        filter(nchar(words)>2)
    }else{
      dt=dt%>%
        tidytext::unnest_tokens(words,sentence,token="words")
    } 
    #filter only negative sentences
    if(is.null(sentiment_corpus)){
      cat(paste0("Please provide a valid sentiment corpus\n by default no sentiment analysis is performed"))
      
      dt=dt%>%group_by(sentence.id)%>%
        summarize(sentence=paste0(words,collapse=" "))%>%
        dplyr::select(sentence)%>%
        mutate(sentence.sentiment=NA)
      
    }else{
      #NS_dt=rio::import("../inst/rawdata/News_sentiments/news_sentiment_replication_code/ns.vader.sentences.20k.csv")
      NS_dt=sentiment_corpus
      NS_dt=NS_dt%>%rename(words=word)
      
      dt=dt%>%left_join(NS_dt,by="words")
      
      dt=dt%>%group_by(sentence.id)%>%
        summarize(sentence=paste0(words,collapse=" "),
                  sentence.sentiment=mean(sentiment,na.rm=T))%>%
        dplyr::select(sentence,sentence.sentiment)
      
      summary_sentiment=dt%>%ungroup()%>%
        mutate(sent=case_when(sentence.sentiment>=max(neutral_sentiment)~"positive",
                              sentence.sentiment>-max(neutral_sentiment)~"neutral",
                              sentence.sentiment<=-min(neutral_sentiment)~"negative"))%>%
        group_by(sent)%>%
        summarize(mean=n())
      
      tot_sentences=dim(dt)[1]
      
      dt=dt%>%filter(sentence.sentiment<sentiment_threshold)%>%
        dplyr::select(sentence,sentence.sentiment)
      cat(mycategory,":", round(dim(dt)[1]/tot_sentences*100)," % of negative sentences, sample:",dim(dt)[1],"\n")
    }
    dt
  })
  fasttextsample=do.call(rbind,fasttextsample)
  fasttextsample=fasttextsample%>%distinct()
  fasttextsample_sentiment=fasttextsample
  fasttextsample=data.frame(sample(fasttextsample[,1]))
  
  # create multilabel sample 
  
  if(multi_label){
    fasttextsample=fasttextsample %>% tibble()%>%
      separate(sentence,into=c("label",'sentence'),sep = "\\s", extra = "merge")%>%
      group_by(sentence)%>% summarize(label=paste0(unique(label),collapse=" "))%>%
      ungroup()%>%
      mutate(sentence=gsub("__","",sentence),
             sentence=paste0(label," ",sentence))%>%
      dplyr::select(sentence)
  }
  
  #reshuffle the dataset
  dt = sort(sample(nrow(fasttextsample), nrow(fasttextsample)*training_size))
  train<-fasttextsample[dt,]
  test<-fasttextsample[-dt,]
  train<-train%>%pull(sentence)
  test<-test%>%pull(sentence)
  
  if(is.null(sentiment_corpus)){
    return(list(train=train,test=test,sentiment=NULL))
  }else{
    return(list(train=train,test=test,sentiment=fasttextsample_sentiment))  
  }
}


ccdr.fasttext.dataforprediction.clean=function(myfile){
  
  #' preprocess the sentences and prepare for prediction
  #' @description clean the sentences by removing numbers,
  #' short sentences, irrelevant sentences and stopwords
  #' @param myfile 
  #' @author manuel betin
  #' @export
  #' 
  myfile=myfile%>%tidytext::unnest_tokens(sentence, paragraph, token = "sentences")
  myfile$sentence.id=1:dim(myfile)[1]    
  # remove numbers from the sentence
  myfile$sentence=str_replace_all(myfile$sentence,"\\d","")
  
  # remove sentences that have less than 15 words and 30 characters
  myfile=myfile%>%
    mutate(n.char=nchar(sentence),
           n.word=str_count(sentence, '\\w+'))%>%
    filter(n.char>30,
           n.word>10)%>%
    dplyr::select(sentence.id,sentence)
  
  #remove stopwords
  myfile=myfile%>%
    tidytext::unnest_tokens(words,sentence,token="words")%>%
    filter(!words %in% stop_words$word)%>%
    group_by(sentence.id)%>%
    summarize(sentence=paste0(words,collapse=" "))%>%
    dplyr::select(sentence) 
  myfile
}

ccdr.fasttext.predict=function(mycorpus,
                               path_rawtext="../../inst/rawdata/rawtext",
                               path_output="../../inst/results",
                               min_sentence_lengh=20,
                               model,k,th){
  
  #' predict the label foreach sentences in the corpus
  #' @description Predict the label of each sentences from a corpus of $
  #' documents provided   
  #' @param mycorpus A list containg the text to be analyzed (output of ccdr.corpus() 
  #' from the CCRr package)
  #' @param path_rawtext Path to the folder that contains the text tokenized in sentences
  #' @param path_output Path where to store the predictions
  #' @param min_sentence_length The threshold for the minimum number of words for a sentence
  #' to be considered
  #' @param ... Arguments to the fasttext_interface (model, k, th) from the package fastText 
  #' @author Manuel Betin
  #' @export
  
  
  mydts=lapply(names(mycorpus),function(x){
    cat("predicting;",x)
    tryCatch({
      myfile=mycorpus[[x]] %>% data.frame()
      colnames(myfile)="paragraph"
      myfile=ccdr.fasttext.dataforprediction.clean(myfile)
      
      if(length(myfile$sentence)<min_sentence_lengh){
        cat(crayon::red("failure, invalid file\n"))
        NULL
      }else{
        writeLines(myfile[,1]$sentence,con=paste0(path_rawtext,"/",x,"_text.txt"))
        
        list_params = list(command = 'predict-prob',
                           model = model,
                           test_data = paste0(path_rawtext,"/",x,"_text.txt"),
                           k = k,
                           th = th)
        
        res = fasttext_interface(list_params, 
                                 path_output = paste0(path_output,"/","predict_valid_prob_",x,".txt"))
        
        if(k>1){
          pred=readLines(con=paste0(path_output,"/","predict_valid_prob_",x,".txt"))
          pred=pred%>%data.frame()
          colnames(pred)="V1"
          mycols=as.vector(outer(c("label","proba_label"), 1:k, paste, sep="_"))
          pred=pred%>%separate(col="V1",into=mycols,sep = " ")
          #pred=rio::import(paste0(path_output,"/","predict_valid_prob_",x,".txt"))
          
          # pred=pred%>%group_by(all_of(mycols))%>%
          #   mutate(V1=ifelse(V2<th,"__label__Nonclassified",V1))%>%
          #   summarize(n=n()/dim(pred)[1])%>%
          #   mutate(file=x)
          cat(crayon::blue(" ok\n"))
          pred  
        }else{
          pred=readLines(con=paste0(path_output,"/","predict_valid_prob_",x,".txt"))
          pred=pred%>%data.frame()
          colnames(pred)="V1"
          pred=pred%>%separate(col="V1",into=c("label","proba_label"),sep = " ")
          
          pred=pred%>%group_by(label)%>%
            summarize(prop=n()/dim(pred)[1])%>%
            mutate(file=x)
          
          cat(crayon::blue(" ok\n"))
          pred  
        }
        
      }
    },
    error=function(e){
      print(e)
      return(NULL)
    })
    
  })
  mydts=do.call(rbind,mydts)
  return(mydts)
  
}



ccdr.fasttext.prediction.spline=function(mypredictions){
  #' correct the missing years in the sample
  #' @description  interpolate missing years with linear 
  #' approximation
  mypredictions=mypredictions%>%
    group_by(iso3,label)%>%
    tidyr::complete(year=1950:2022%>%as.character())%>%
    group_by(iso3,label)%>%
    mutate(prop=na.approx(prop,na.rm=F))
  return(mypredictions)
}
