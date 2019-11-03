\name{invers_doc_frequency}
\alias{invers_doc_frequency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute the inverse document frequency as the logarithm of the inverse of the proportion. it allows to give reduce weights of words with high frequency in the corpus
}
\description{
compute the inverse document frequency as the logarithm of the inverse of the proportion. it allows to give reduce weights of words with high frequency in the corpus
\usage{
invers_doc_frequency(table_N_occurence, group = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_N_occurence}{
output of table_brute_frequency_vector()
}
  \item{group}{
the name of one of the column of the dataframe on which you want to group the computation
}
}
\details{
}
\value{
the function returns a dataframe with categories in rows and a column with the index

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
function (table_N_occurence, group = NULL) 
{
    N.doc.corpus = dim(table_N_occurence)[1]
    idf_trans = function(x) {
        if (x == 0) {
            x
        }
        else {
            log(N.doc.corpus/sum(x))
        }
    }
    table_N_binary = binary_frequency_trans(table_N_occurence)
    if (!is.null(group)) {
        inverse_doc_freq = table_N_binary \%>\% group_by(get(group)) \%>\% 
            summarize_if(is.numeric, sum) \%>\% mutate_if(is.numeric, 
            idf_trans) \%>\% gather("Crisis", value = "idf", -"get(group)")
        colnames(inverse_doc_freq)[1] = group
    }
    else {
        inverse_doc_freq = table_N_binary \%>\% summarize_if(is.numeric, 
            sum) \%>\% mutate_if(is.numeric, idf_trans) \%>\% gather("Crisis", 
            value = "idf")
    }
    return(inverse_doc_freq)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
