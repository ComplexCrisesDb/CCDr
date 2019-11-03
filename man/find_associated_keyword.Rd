\name{find_associated_keyword}
\alias{find_associated_keyword}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
provide the keywords associated to each category of crisis
}
\description{
provide the keywords associated to each category of crisis
}
\usage{
find_associated_keyword(keyword)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (keyword) 
{
    detect = names(key_words_crisis())[str_detect(tolower(names(key_words_crisis())), 
        tolower(keyword))]
    key_words_crisis()[detect]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
