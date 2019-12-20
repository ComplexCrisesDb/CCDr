\name{tf_vector}
\alias{tf_vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Vectorize the function tf() to be able to pass a list of names of keywords keyword_list is a list containing the names of different groups of keywords that have a vector of words to look into.
}
\description{
vectorize the function tf() to be able to pass a list of names of keywords keyword_list is a list containing the names of different groups of keywords that have a vector of words to look into.
}
\usage{
tf_vector(corpus, keyword_list, brute_freq = F, parrallel = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus}{
a list of texts from pdf_text() with different names for each element
}
  \item{keyword_list}{
the names of the items in keyword_list_crisis to include in the computation
}
  \item{brute_freq}{
the type of frequency to choose
}
  \item{parrallel}{
T/F whether to use mcparrallel to perform parrallel computing (only available on mac/linux)
}
}
\value{
the function returns a table with each row corresponding to a document and each colum providing the number of occurence for the given item.
}
\author{
Manuel Betin
}