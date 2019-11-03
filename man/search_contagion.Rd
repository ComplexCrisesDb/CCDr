\name{search_contagion}
\alias{search_contagion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
provide the keywords associated to each category of crisis
}
\description{
provide the keywords associated to each category of crisis
}
\usage{
search_contagion(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
a character string where to check for mentions of other countries 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns a dataframe with the country names and mnemonics
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
    tic()
    ctry_table = keywords_country_contagion()
    ctries = c(ctry_table$ctries, ctry_table$adjectival, ctry_table$Currency, 
        ctry_table$Symbol, key_words_crisis()$Crisis_contagion)
    text = clean_text(file)
    text_df <- tibble(text = text) \%>\% unnest_tokens(word, text) \%>\% 
        anti_join(stop_words, by = c("word"))
    toc()
    text_df \%>\% filter(word \%in\% tolower(ctries)) \%>\% count(word) \%>\% 
        arrange(word) \%>\% filter(n > 1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
