\name{table_brute_frequency}
\alias{table_brute_frequency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
for each document in the corpus the function creates a table counting the occurence of the words in the vector of keywords and provide a table with the number of occurence,name of the file that is the name of each document in the corpus (from which we can extract: the country code, period, type of program and year)
}
\description{
for each document in the corpus the function creates a table counting the occurence of the words in the vector of keywords and provide a table with the number of occurence,name of the file that is the name of each document in the corpus (from which we can extract: the country code, period, type of program and year)
}
\usage{
table_brute_frequency(corpus, keywords, brute_freq = F, parrallel = T)
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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns a table with the name of the files in rows and a columns with the counting of words

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
## The function is currently defined as
function (corpus, keywords, brute_freq = F, parrallel = T) 
{
    require(tidytext)
    require(stringi)
    require(stringr)
    if (is.vector(keywords) & !is.null(keywords)) {
        table = lapply(corpus, function(x) {
            output = try(sum(Eval_targeted_pages(x, keywords, 
                brute_freq = brute_freq, parrallel = parrallel)[, 
                1]))
            if ("try-error" \%in\% class(output)) {
                print(paste0("Warning: error when mining ", keywords))
                output = 0
            }
            output
        })
        names(table) = names(corpus)
        table = do.call(rbind, table)
        table = tibble(table)
        table$file = names(corpus)
        names(table) = c("var", "file")
        table
    }
    else {
        print("please provide a vector of strings as argument for keywords")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
