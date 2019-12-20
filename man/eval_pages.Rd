\name{eval_pages}
\alias{eval_pages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
provide list of files and return summary of counts of occurence of the target world 
}
\description{
provide list of files and return summary of counts of occurence of the target world 
}
\usage{
eval_pages(files, targetword, brute_freq = F, parrallel = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{
a list of string of characters where to perform the search
}
  \item{targetword}{
the target word to locate in the file
}
  \item{brute_freq}{
the type of frequency to choose
}
  \item{parrallel}{
T/F whether to use mcparrallel to perform parrallel computing (only available on mac/linux)
}
}
\value{
the function return the number of occurence for each file
}
\author{
Manuel Betin
}