\name{aggregate_corpus}
\alias{aggregate_corpus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function that takes the path of the directory and load all the pdfs of the directory into a list in order to further perform the text mining
}
\description{
function that takes the path of the directory and load all the pdfs of the directory into a list in order to further perform the text mining
}
\usage{
aggregate_corpus(path_files,only_files=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_files}{
the path of the directory with the files 
}
  \item{only_files}{
T/F, if F the function returns the files and the metadata of the pdf
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a list containing the content of each document
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Manuel Betin
}
