\name{find_pertinance}
\alias{find_pertinance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
provide the keywords associated to each category of crisis
}
\description{
provide the keywords associated to each category of crisis
}
\usage{
find_pertinance(table.N.occurrence, keyword)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table.N.occurrence}{
tf.idf dataframe (output of table_word_weight())
}
  \item{keyword}{
one of the names of category in key_words_crisis()
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns a vector of strings corresponding to the selected categories

}
\author{
Manuel Betin
}