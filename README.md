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

urls_names=c('name_file','ID',' period',' title',' pdf')

urls_data1=c('ARG_1980-07-25','ARG',' 1980-07-25',' argentina - recent economic developments',' https://imfbox.box.com/shared/static/ll8fltakl55iz79yc9d1bzdjtgx42fw0.pdf')
urls_data2=c('ARG_1981-07-25','ARG',' 1980-07-25',' argentina - recent economic developments',' https://imfbox.box.com/shared/static/ll8fltakl55iz79yc9d1bzdjtgx42fw0.pdf')

dt=data.frame(rbind(urls_data1,urls_data2))
names(dt)=urls_names

run_tf_by_chunk(urls=dt, keyword_list=c("Currency_crisis"), extract_number="name_file", delete_pdfs = F)

