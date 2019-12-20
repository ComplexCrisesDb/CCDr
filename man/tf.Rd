\name{tf}
\alias{tf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
for each document in the corpus the function creates a table counting the occurence of the words in the vector of keywords and provide a table with the number of occurence,name of the file that is the name of each document in the corpus (from which we can extract: the country code, period, type of program and year)
}
\description{
for each document in the corpus the function creates a table counting the occurence of the words in the vector of keywords and provide a table with the number of occurence,name of the file that is the name of each document in the corpus (from which we can extract: the country code, period, type of program and year)
}
\usage{
tf(corpus, keywords, brute_freq = F, parrallel = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus}{
 a list of texts from pdf_text() with different names for each element
}
  \item{keywords}{
a vector of strings containing the targeted words to look for
}
  \item{brute_freq}{
the type of frequency to choose
}
  \item{parrallel}{
T/F whether to use mcparrallel to perform parrallel computing (only available on mac/linux)
}
}
\value{
the function returns a table with the name of the files in rows and a columns with the counting of words

}
\author{
Manuel Betin
}