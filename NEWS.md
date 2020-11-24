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


TextMiningCrisis v1.1.0 (Release date: 2020-02-13)
==============

Changes:

* correction in the function find_pages() replace tokenization by ngram with tokenization by sentences.


TextMiningCrisis v1.1.1 (Release date: 2020-02-13)
==============

Changes:

* include argument loc_temp in the function run_tf_by_chunk() to specify the location of the temporary file and be able to change
the default folder that is the root directory.

TextMiningCrisis v1.1.2 (Release date: 2020-02-14)
==============

Changes:

* Update of the lexicon of economic crisis with more precise categories for currecncy crisis and types of documents.


TextMiningCrisis v1.2.2 (Release date: 2020-02-20)
==============

Changes:

* deal better with special characters in the lexicon and include netting of confusion sentences in the tf_vector function. 
find_page() now tokenize by sentences an no longer by n-gram.

TextMiningCrisis v1.2.3 (Release date: 2020-03-03)
==============

Changes:

* Include new indexes: one for banking_crisis_severe that mimics currency_crisis_severe, epidemics that contains words related to epidemy. tf function is set as to disregard documents that are not relevant such as minutes of executive boards


TextMiningCrisis v1.3.0 (Release date: 2020-03-11)
==============

Changes:

* change the name of some functions to :
 - plot_cos_sim() => cosim_fig()
 - key_word_crisis() => lexicon() 
 - typology_categories() => lexicon_typology()
 - find_associated_keywords() => lexicon_details()
 - country_radar_dt() => radar_dt()
 - country_radar_fig() => radar_shocks_fig() 
 - find_pages() => get_pages()


TextMiningCrisis v1.3.1 (Release date: 2020-03-23)
==============

update aggregate_corpus() with the argument ENGINE that can be either "pdf_text" to use pdf_text() fonction from pdftools package to read the pdf files or "pdf_ocr_text" from tesseract packages. using "pdf_ocr_text" allows to more precisely read pdf that are scanned documents. Warning: using pdf_ocr_text increase very substantially the computing time.

Include vignette to discribe the use of the package



TextMiningCrisis v1.3.1 (Release date: 2020-03-26)
==============

New function get_imf_country_report() that download the last imf country reports from the imf website.


TextMiningCrisis v1.4.0 (Release date: 2020-11-20)
==============

Clean package to only keep relevant functions to construct the database of indexes, use Depends instate of Imports to ensure dependencies are correctly loaded.
Include website for the package and update readme.



 



