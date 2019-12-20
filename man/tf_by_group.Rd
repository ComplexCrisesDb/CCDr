\name{tf_by_group}
\alias{tf_by_group}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute the average importance of each crisis by groups
}
\description{
compute the average importance of each crisis by groups
}
\usage{
tf_by_group(table_N_occurence, weight_method, mygroup)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_N_occurence}{
tf.idf dataframe (output of tft())
}
  \item{weight_method}{
one of "brut_frequency", "binary_frequency","log_norm_frequency"
}
  \item{mygroup}{
name of the column to group the data
}
}
\value{
the function returns a summary dataframe with average tf.idf for each category and group
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Manuel Betin
}
