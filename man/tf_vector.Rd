\name{tf_vector}
\alias{tf_vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
vectorize the function table_brute_frequency() to be able to pass a list of names of keywords keyword_list is a list containing the names of different groups of keywords that have a vector of words to look into.
}
\description{
vectorize the function table_brute_frequency() to be able to pass a list of names of keywords keyword_list is a list containing the names of different groups of keywords that have a vector of words to look into.
}
\usage{
tf_vector(corpus, keyword_list, brute_freq = F, parrallel = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus}{
a list of texts from pdf_text() with different names for each element
}
  \item{keyword_list}{
the names of the items in keyword_list_crisis to include in the computation
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
the function returns a table with each row corresponding to a document and each colum providing the number of occurence for the given item.
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
function (corpus, keyword_list, brute_freq = F, parrallel = T) 
{
    list_table_keyword_occurence = lapply(1:length(keyword_list), 
        function(x) {
            print(paste0("running: ", names(keyword_list)[x]))
            tic()
            if (!"character" \%in\% class(keyword_list[[x]])) {
                warning("please provide a valid vector of characters")
                dt = NULL
                toc()
                dt
            }
            else {
                res = try({
                  dt = tf(corpus, keyword_list[[x]], 
                    brute_freq = brute_freq, parrallel = parrallel)
                  dt = dt \%>\% rename(`:=`(!!paste0(names(keyword_list)[x]), 
                    var))
                  dt
                })
                if ("try-error" \%in\% class(dt)) {
                  res = NULL
                }
                toc()
                res
            }
        })
    names(list_table_keyword_occurence) = names(keyword_list)
    dt = list_table_keyword_occurence[[1]]
    for (i in 2:length(list_table_keyword_occurence)) {
        res = try({
            dt = dt \%>\% left_join(list_table_keyword_occurence[[i]], 
                by = c("file"))
            dt
        })
        if ("try-error" \%in\% class(res)) {
            dt = dt
        }
        else {
            dt = res
            dt = dt \%>\% dplyr::select(file, everything()) \%>\% 
                distinct()
        }
    }
    return(dt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
