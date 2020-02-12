\name{find_pages}
\alias{find_pages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Provide a files either pdf of html and get the paragraphs matching the targetted word
}
\description{
 provide files either pdf of html and return the paragraphs matching the targetted word
}
\usage{
find_pages(file, targetword,n_ngram=10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
a string of characters where to perform the search
}
  \item{targetword}{
the target word to locate in the file
}
\item{n_ngram}{
Number of words in the ngram used for the tokenization.
}
}
\value{
the function return the target word provided, the number of occurence by page, the total occurence in the document and the
pages of the document that contain the words 
}
\author{
Manuel Betin
}