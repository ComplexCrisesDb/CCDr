TextMiningCrisis v1.0.2 (Release date: 2020-01-23)
==============

Changes:

* add new function: run_tf_by_chunk() that allows to provide the list of urls and the keywords of the keywords to detect and sequentially
download the pdf files from the web, consolidate the corpus, create the tf database, remove the original pdf files and store the corpus and 
the tf into a folder called temp. Usefull function for very large list of pdf to avoid storage limitation.


TextMiningCrisis v1.0.3 (Release date: 2020-01-30)
==============

Changes:

* Clean code, remove few useless functions, clean documentation and solve warnings and notes when building the package


TextMiningCrisis v1.0.4 (Release date: 2020-02-03)
==============

Changes:

* update run_tf_chunk() to include two parameters to trim the documents that contain less than a specified number of words. usefull
to clean the corpus of irrelevant documents.


TextMiningCrisis v1.0.5 (Release date: 2020-02-03)
==============

Changes:

* Include an example html to show how functions works.



TextMiningCrisis v1.0.6 (Release date: 2020-02-05)
==============

Changes:

* Include properly the database of urls to the package: IMF_docs_urls



TextMiningCrisis v1.0.7 (Release date: 2020-02-12)
==============

Changes:

* Include an additional parameter (n_ngram=10) to find_pages() to control the number of words in the ngrams used when tokenizing the 
files. The default value is set to 10 and replace the default set to 3 in previous versions.


