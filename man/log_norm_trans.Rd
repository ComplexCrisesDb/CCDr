\name{log_norm_trans}
\alias{log_norm_trans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 log normal transformation of the table
}
\description{
 log normal transformation of the table
}
\usage{
log_norm_trans(table_N_occurence)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_N_occurence}{
output of table_brute_frequency_vector()
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns the log normal transformation of the dataframe.

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
function (table_N_occurence) 
{
    log_norm_trans = function(x) {
        ifelse(x > 0, 1 + log(x), 0)
    }
    table_log_norm_trans = table_N_occurence \%>\% mutate_if(is.numeric, 
        log_norm_trans)
    return(table_log_norm_trans)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
