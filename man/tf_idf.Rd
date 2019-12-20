\name{tf_idf}
\alias{tf_idf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to compute the weights of each word by doing the product of the index of frequency and the inverse
  document frequency
}
\description{
function to compute the weights of each word by doing the product of the index of frequency and the inverse
  document frequency
}
\usage{
tf_idf(table_N_occurence, weight_method = "brut_frequency")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_N_occurence}{
output of table_brute_frequency_vector()
}
  \item{weight_method}{
one of "brut_frequency", "binary_frequency","log_norm_frequency"
}
}
\value{
The function returns a dataframe with categories in rows and a column with the tf.idf
}
\author{
Manuel Betin
}