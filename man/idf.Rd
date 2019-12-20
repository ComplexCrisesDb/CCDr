\name{idf}
\alias{idf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute the inverse document frequency as the logarithm of the inverse of the proportion. it allows to give reduce weights of words with high frequency in the corpus
}
\description{
compute the inverse document frequency as the logarithm of the inverse of the proportion. it allows to give reduce weights of words with high frequency in the corpus
}
\usage{
idf(table_N_occurence, group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_N_occurence}{
  output of tf_vector
}
  \item{group}{
  the name of one of the column of the dataframe on which you want to group the computation
 }
}
\value{
the function returns a dataframe with categories in rows and a column with the index
}
\author{
Manuel Betin
}