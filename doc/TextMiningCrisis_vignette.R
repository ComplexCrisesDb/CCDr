## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(eval = F)

## ----warning=FALSE-------------------------------------------------------
#  TextMiningCrisis::lexicon() [c("Natural_disaster","Wars")]

## ----warning=FALSE,results='hide'----------------------------------------
#  ## Aggregate files in docs_example directory (IMF documents for Argentina 1980-1990):
#  
#  #corpus <- TextMiningCrisis::aggregate_corpus("../data/docs_example", only_files = T)

## ------------------------------------------------------------------------
#  # Show the second page of the first document:
#  data("IMF_corpus_example")
#  corpus=IMF_corpus_example
#  corpus[[1]][[2]]

## ----warning=FALSE,results='hide'----------------------------------------
#  
#  
#  # Create a term-frequency matrix for previous corpus:
#  data("IMF_tf_example")
#  
#  tf <-TextMiningCrisis::run_tf("../data/corpus_example.RData",keyword_list = c("Currency_crisis_severe","Wars"))
#  

## ---- echo=F-------------------------------------------------------------
#  # Display matrix:
#  tf

## ---- include=FALSE------------------------------------------------------
#  # Load packages used in this session:
#  
#  library(ggplot2)
#  library(tidyverse)

## ----fig.width=7---------------------------------------------------------
#  
#  # Extract year and iso3, average over documents by year:
#  
#  tf_year <- tf %>%
#    mutate(year = str_extract(file,"\\d{4}")) %>%
#    mutate(iso3c = str_extract(file,"[A-Z]{3}")) %>%
#    group_by(iso3c,year) %>%
#    summarize_at(vars(c("Currency_crisis_severe","Wars")), mean, na.rm = TRUE) %>%
#    gather("tf_type","tf_value",Currency_crisis_severe:Wars)
#  
#  # Plot:
#  
#  tf_year %>%
#    ggplot(aes(year,tf_value*100,col=tf_type,group = 1)) +
#    geom_line() +
#    facet_wrap(~tf_type) +
#    theme_bw() +
#    ylab("%") +
#    xlab("") +
#    theme(legend.position = "none") +
#    theme(axis.text.x = element_text(angle = 90, hjust = 1))
#  
#  

## ------------------------------------------------------------------------
#  TextMiningCrisis::lexicon()[c("Problematic_documents")] %>% data.frame() %>% filter(str_detect(Problematic_documents, "working"))

## ------------------------------------------------------------------------
#  TextMiningCrisis::lexicon()[c("Currency_crisis_confusing")] %>% data.frame()

## ------------------------------------------------------------------------
#  
#  sentences_currency_crisis <- TextMiningCrisis::get_sentences(corpus, c("Currency_crisis_severe"))
#  sentences_currency_crisis[["ARG_1989-10-17_request"]]
#  

## ------------------------------------------------------------------------
#  # Extract the third sentence with keyword detected in a randomly chosen 1989 document:
#  
#  sentences_currency_crisis[["ARG_1989-10-17_request"]] %>%
#    select(sentence) %>%
#    slice(3) %>%
#    data.frame()
#  

## ----echo=T,results='hide'-----------------------------------------------
#  # In this example we add an index for natural disasters events to the previous extraction:
#  
#  tf_path = "../data/tf_crisis_words_example.RData"
#  corpus_path = "../data/corpus_example.RData"
#  
#  updated_tf <-TextMiningCrisis::run_tf_update(path_tf_to_update = tf_path, corpus_path = corpus_path, keyword_list = c("Natural_disaster"))

## ----echo=F,fig.width=7--------------------------------------------------
#  
#  # Average by year and plot as before:
#  
#  updated_tf_year <- updated_tf %>%
#    mutate(year = str_extract(file,"\\d{4}")) %>%
#    mutate(iso3c = str_extract(file,"[A-Z]{3}")) %>%
#    group_by(iso3c,year) %>%
#    summarize(Natural_disaster = mean(Natural_disaster, na.rm = TRUE))
#  
#  updated_tf_year %>%
#    ggplot(aes(year,Natural_disaster*100,col = iso3c, group = 1)) +
#    geom_line() +
#    theme_bw() +
#    ylab("%") +
#    xlab("") +
#    theme(legend.position = "none") +
#    theme(axis.text.x = element_text(angle = 90, hjust = 1))
#  
#  TextMiningCrisis::get_sentences(corpus, c("Natural_disaster"))[["ARG_1988-02-26_Use fund"]] %>% select(sentence) %>% slice(10) %>% data.frame()
#  
#  
#  

