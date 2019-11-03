\name{country_radar_dt}
\alias{country_radar_dt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Look at average importance of each crisis by country and display the more important elements
}
\description{
Look at average importance of each crisis by country and display the more important elements
}
\usage{
country_radar_dt(table_N_occurence, isoc, top_n = 50, weight_method = "brut_frequency", group = "ISO3_Code")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_N_occurence}{
tf.idf dataframe (output of table_word_weight())
}
  \item{isoc}{
the iso code of the country of interest
}
  \item{top_n}{
the maximum number of categories to display
}
  \item{weight_method}{
one of "brut_frequency", "binary_frequency","log_norm_frequency"
}
  \item{group}{
%%     ~~Describe \code{group} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns a dataframe
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
function (table_N_occurence, isoc, top_n = 50, weight_method = "brut_frequency", 
    group = "ISO3_Code") 
{
    table = evol_weight_by_group(table_N_occurence, weight_method = weight_method, 
        group = group)
    table \%>\% filter(ISO3_Code \%in\% isoc) \%>\% arrange(-word_weight) \%>\% 
        top_n(top_n)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
