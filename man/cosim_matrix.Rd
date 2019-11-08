\name{cosim_matrix}
\alias{cosim_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute the cosinus similarity matrix by comparing pairwise columns
}
\description{
compute the cosinus similarity matrix by comparing pairwise columns
}
\usage{
cosim_matrix(table_weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_weights}{
tf.idf dataframe (output of table_word_weight())
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns a symetric matrix with the cosinus similarity of each pair of categories
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
function (table_weights) 
{
    vects = names(table_weights \%>\% dplyr::select_if(is.numeric))
    res = matrix(NA, length(vects), length(vects), dimnames = list(vects, 
        vects))
    for (j in 1:length(vects)) {
        for (i in 1:length(vects)) {
            res[i, j] = cosim(table_weights, vects[j], 
                vects[i])
        }
    }
    res[res == "Inf"] = 0
    res[res == "NaN"] = 0
    res = res \%>\% round(., 2)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
