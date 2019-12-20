\name{ngram_clean}
\alias{ngram_clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
generate tibble with ngrams of specified length in rows and dummies for  a set of typologies, the function clean the text by removing unnecessary characters and removing common stop words.
}
\description{
generate tibble with ngrams of specified length in rows and dummies for  a set of typologies, the function clean the text by removing unnecessary characters and removing common stop words.

}
\usage{
ngram_clean(text, length_ngram = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
a character string containing the text to analyse
}
  \item{length_ngram}{
the number of word to consider has the unit of observation
}
}

\value{
a tibble with ngrams in rows, number of occurence and dummies for the typologies 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Manuel Betin
}
