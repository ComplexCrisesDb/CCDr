\name{run_tf}
\alias{run_tf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function that compute the tf matrix for a corpus given in a list format, the type of lexicon to use and the sublist of keywords associated
}
\description{
function that compute the tf matrix for a corpus given in a list format, the type of lexicon to use and the sublist of keywords associated
}
\usage{
run_tf(corpus_path = paste0(root_path, "/3. Data/IMF Letters of Intents/IMF_letofIntent_1960_2014_clean.RData"), 
      type_lexicon = "words", keyword_list = c("Commodity_crisis", "Balance_payment_crisis", "Inflation_crisis"), 
      export_path = NULL, parrallel = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus_path}{
path where to locate the RData file containing the list of documents (in string of characters)
}
  \item{type_lexicon}{
one of "words" or "categories"
}
  \item{keyword_list}{
the names of the categories (in key_words_crisis() or key_words_categories())
}
  \item{export_path}{
where to export the resulting dataframe
}
  \item{parrallel}{
T/F to use mcparralle (only available on mac/linux)
}
}
\value{
The output is a matrix of tf with a row per document and a column for each element of the keyword list 
}
\author{
Manuel Betin
}