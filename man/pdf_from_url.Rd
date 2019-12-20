\name{pdf_from_url}
\alias{pdf_from_url}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
download a list of pdf files from a list of urls
}
\description{
Usefull function to scrap pdf documents from a list of urls. The function will create a directory if not existing and download each element
}
\usage{
pdf_from_url(urls,export_path,overwrite=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{urls}{
  a dataframe containing at least two columns: name_file (name of each file) and pdf (valid url link to the pdf file)
}
 \item{export_path}{
 path where to download the files
}
\item{overwrite}{
T/F, if F then the the function will not download the file if already existing
}

\details{
download the files in a specified directory or if not in the root directory, creates a RData file with the log of the download
}
\author{
Manuel Betin
}