\name{cosinus_similarity}
\alias{cosinus_similarity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
formula for the cosinus similiarity that provides the distance between two vectors here two crisis
}
\description{
formula for the cosinus similiarity that provides the distance between two vectors here two crisis
}
\usage{
cosinus_similarity(table_N_occurence, vec1, vec2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_N_occurence}{
output of table_brute_frequency_vector()
}
  \item{vec1}{
vector of tf.idf
}
  \item{vec2}{
vector of tf.idf
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns a numeric value corresponding to the cosinus similarity
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
function (table_N_occurence, vec1, vec2) 
{
    res = table_N_occurence \%>\% dplyr::select(vec1, vec2) \%>\% 
        mutate(scalar_prod = get(vec1) * get(vec2), norm_1 = (get(vec1))^2, 
            norm_2 = (get(vec2))^2) \%>\% summarize(scalar_prod = sum(scalar_prod), 
        norm_1 = sum(norm_1), norm_2 = sum(norm_2)) \%>\% mutate(cosinus_similarity = scalar_prod/(sqrt(norm_1) * 
        sqrt(norm_2))) \%>\% dplyr::select(cosinus_similarity)
    return(res$cosinus_similarity)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
