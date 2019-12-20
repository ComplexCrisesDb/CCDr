\name{run_tf_update}
\alias{run_tf_update}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function that update the tf  output of run_tf with the new variables, avoiding recomputing all the categories
}
\description{
function that update the tf  output of run_tf with the new variables, avoiding recomputing all the categories
}
\usage{
run_tf_update(path_tf_to_update = paste0(root_path, "/3. Data/IMF Letters of Intents/tf_crisis_words.RData"), 
              corpus_path = paste0(root_path, "/3. Data/IMF Letters of Intents/IMF_letofIntent_1960_2014_clean.RData"), 
              type_lexicon = "words", keyword_list = NULL,
              export_path = paste0(root_path, "/3. Data/IMF Letters of Intents/tf_crisis_words.RData"), parrallel = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_tf_to_update}{
path_tf_to_update_path: path where to locate the RData file containing the tf dataframe to update
}
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