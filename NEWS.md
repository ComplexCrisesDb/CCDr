TextMiningCrisis v1.0.2 (Release date: 2020-01-23)
==============

Changes:

* add new function: run_tf_by_chunk() that allows to provide the list of urls and the keywords of the keywords to detect and sequentially
download the pdf files from the web, consolidate the corpus, create the tf database, remove the original pdf files and store the corpus and 
the tf into a folder called temp. Usefull function for very large list of pdf to avoid storage limitation.


