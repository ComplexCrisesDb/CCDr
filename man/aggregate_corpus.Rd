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
aggregate_corpus(path_files)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_files}{
the path of the directory with the files 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a list containing the content of each document
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (path_files) 
{
    library(pdftools)
    docs = list.files(path_files, pattern = ".pdf")
    docs = str_remove(docs, ".PDF")
    docs = str_remove(docs, ".pdf")
    count = 0
    start = 1
    x = 1
    corpus = lapply(start:length(docs), function(x) {
        tic(docs[x])
        path = paste0(path_files, "/", docs[x], ".PDF")
        file <- try({
            pdfinfo = pdf_info(path)
            pdf_text(path) \%>\% strsplit(split = "\n")
        }, silent = T)
        if ("try-error" \%in\% class(file)) {
            warning(paste(docs[[x]], ": Error in path file", 
                sep = ""))
            file = NA
        }
        else {
            file = clean_text(file)
        }
        count <- count + 1
        print(path)
        print(count)
        toc()
        list(info = pdfinfo, file = file)
    })
    names(corpus) = docs
    return(corpus)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{}% use one of  RShowDoc("KEYWORDS")
\keyword{}% __ONLY ONE__ keyword per line
