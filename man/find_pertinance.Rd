\name{find_pertinance}
\alias{find_pertinance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
provide the keywords associated to each category of crisis
}
\description{
provide the keywords associated to each category of crisis
}
\usage{
find_pertinance(table.N.occurrence, keyword)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table.N.occurrence}{
tf.idf dataframe (output of table_word_weight())
}
  \item{keyword}{
one of the names of category in key_words_crisis()
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns a vector of strings corresponding to the selected categories

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
function (table.N.occurrence, keyword) 
{
    words = find_associated_keyword(keyword)
    table = table_word_weight(table.N.occurrence)
    table = table \%>\% gather(Crisis, value = "weight", -c(ISO3_Code, 
        Period, year, type_prog, file))
    results = table \%>\% filter(str_detect(tolower(Crisis), tolower(keyword)) & 
        weight > 0) \%>\% arrange(-weight)
    return(list(keywords = words, results = results))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
