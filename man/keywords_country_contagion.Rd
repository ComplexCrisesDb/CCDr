\name{keywords_country_contagion}
\alias{keywords_country_contagion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
provide the keywords associated to each category of crisis
}
\description{
provide the keywords associated to each category of crisis
}
\usage{
keywords_country_contagion()
}
%- maybe also 'usage' for other objects documented here.
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
## The function is currently defined as
function () 
{
    if (curl::has_internet()) {
        path = "https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations"
        html_file <- try(read_html(path), silent = T)
        ctry_table = html_file \%>\% html_nodes(xpath = "//*[@id=\"mw-content-text\"]/div/table[1]") \%>\% 
            html_table(fill = T)
        ctry_table = ctry_table[[1]]
        ctry_table = data.frame(ctry_table)[-1, ]
        colnames(ctry_table) = c("ctries", "adjectival", "Demonyms", 
            "Demonyms_2", "X")
        path = "https://en.wikipedia.org/wiki/List_of_circulating_currencies"
        html_file <- try(read_html(path), silent = T)
        currency_table = html_file \%>\% html_nodes(xpath = "//*[@id=\"mw-content-text\"]/div/table[1]") \%>\% 
            html_table(fill = T)
        currency_table = currency_table[[1]]
        names(currency_table) = c("ctries", "Currency", "Symbol", 
            "ISO2_Code", "fractional_unit", "basis")
        table = left_join(x = ctry_table, y = currency_table, 
            by = "ctries")
    }
    else {
        ctry_table = rio::import("../3. Data/List countries/wikipedia_country_names.RData")
    }
    return(table)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
