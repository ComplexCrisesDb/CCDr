# TextMiningCrisis 

***
***

last update: 30/01/2020

# Description

Package containing a set of functions to perform a supervised text mining using a lexicon of economic crisis to observe the profile and intensity of economic crisis in a text document.

# Author

- Manuel Betin

# current version:

 1.0.3
 
 # usage
 
key functions are
 
 # example

```r 
library(dplyr)
library(rio)
library(DT)
library(TextMiningCrisis)
```

## Load the data containing the urls

```r

set.seed(2)

url_links=rio::import("../data/urls_Requests_Reviews_articleIV.RData") %>%
  mutate(name_file=paste0(ID,"_",period,"_",type_doc_programs))

url_links= url_links %>% filter(ID=="ARG")
url_links=url_links[150:155,]

DT::datatable(url_links[,1:5])
```


## Download the files and store in folter "mydocs_to_textmining"

```r

pdf_from_url(url_links,"mydocs_for_textmining")

```


## aggregate content of pdfs in folder "mydocs_to_textmining" into a single corpus 

```r

corpus=aggregate_corpus("mydocs_for_textmining",only_files = T)
save(corpus,file="mycorpus.RData")
```

## Find the number of occurence of the word "cotton" by paragraph 

```r
doc_example=corpus[4]
Number_pages_containing_word=eval_pages(doc_example,"debt")

```

## Find the paragraphs containing the word "cotton" by paragraph

```r
pages_containing_word=find_pages(doc_example,"debt")

```

## compute the document term frequency for all the files in the corpus for the category "Currency_crisis"

```r

tf_matrix=tf(corpus,"Currency_crisis")

DT::datatable(head(tf_matrix))
```


## compute the document term frequency for several categories "Currency_crisis" and "Balance_payment_crisis"

```r

# term frequency matrix for several categories of crisis
mycategories=c('Currency_crisis',"Balance_payment_crisis")
tf_matrix_with_several_categories=tf_vector(corpus,key_words_crisis()[mycategories])

DT::datatable(head(tf_matrix_with_several_categories))
```

## Wrapup function for tf

```r
#run term frequency matrix

wrapup_for_tf=run_tf(corpus_path = "mycorpus.RData",type_lexicon ="words",keyword_list = c("Currency_crisis","Balance_payment_crisis"),parrallel = F)

DT::datatable(head(wrapup_for_tf))
```

# Wrapup function for run_tf that allows directly download the files and run the text mining with a single function

```r
#wrapup of all preivous functions to download and mine a list of urls directing to the pdfs of interests
run_tf_by_chunk(urls =url_links,keyword_list = c("Currency_crisis","Balance_payment_crisis"))

```

# Update the tf dataframe with additional columns with the new categories to compute

```r

updated_tf=run_tf_update(path_tf_to_update = "temp/tf/tf_crisis_words_1.RData",
                corpus_path = "temp/corpus/corpus_1.RData",
                keyword_list = c("Fiscal_outcomes","Fiscal_consolidation"),
                export_path = "temp/tf/tf_crisis_words_1_new.RData")

DT::datatable(head(updated_tf))
```


