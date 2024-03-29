% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/term_frequencies.R
\name{ccdr.tfs}
\alias{ccdr.tfs}
\title{vectorize the function tf() to be able to pass a list of names of keywords
keyword_list is a list containing the names of different groups of
keywords that have a vector of words to look into.}
\usage{
ccdr.tfs(
  corpus,
  lexicon,
  brute_freq = F,
  parrallel = T,
  centre_countries = "USA"
)
}
\arguments{
\item{corpus}{a list of texts from pdf_text() with different names
for each element}

\item{lexicon}{a lexicon in the form of a list containing a vector of keywords, 
the list is named with the name of the category of interest}

\item{brute_freq}{T/F if T it will just count the occurence, otherwise
it will compute the term frequency}

\item{parrallel}{T/F if T it will use mclapply from the parrallel package}

\item{centre_countries}{Character string. Default is "USA". Netting of confusing
keywords will not be performed countries in the vector.}
}
\value{
a tibble with the term frequencies for the selected categories
}
\description{
vectorize the function tf() to be able to pass a list of names of keywords
keyword_list is a list containing the names of different groups of
keywords that have a vector of words to look into.
}
\author{
Manuel Betin
}
