\name{cosim_matrix}
\alias{cosim_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the cosinus similarity matrix by comparing pairwise columns
}
\description{
Compute the cosinus similarity matrix by comparing pairwise columns
}
\usage{
cosim_matrix(table_weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_weights}{
tf.idf dataframe (output of table_word_weight())
}
\value{
the function returns a symetric matrix with the cosinus similarity of each pair of categories
}
\author{
Manuel Betin
}