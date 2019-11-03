\name{clean_text}
\alias{clean_text}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
clean a string of character by removing special characters, tabulations, very short paragraphs...

}
\description{
clean a string of character by removing special characters, tabulations, very short paragraphs...

}
\usage{
clean_text(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
a string of characters to clean
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a string of characters cleanned
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
function (file) 
{
    file = str_replace_all(file, "[\\s]+", " ")
    file = gsub("\r", "", file)
    file = gsub("\"", "", file)
    file = gsub("\n", "", file)
    file = file[str_count(file) > 50]
    file = gsub("<U+23AF>", "", file)
    file = tolower(file)
    file
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
