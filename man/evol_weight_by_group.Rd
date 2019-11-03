\name{evol_weight_by_group}
\alias{evol_weight_by_group}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute the average importance of each crisis by groups
}
\description{
compute the average importance of each crisis by groups
}
\usage{
evol_weight_by_group(table_N_occurence, weight_method = "brut_frequency", group = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_N_occurence}{
tf.idf dataframe (output of table_word_weight())
}
  \item{weight_method}{
one of "brut_frequency", "binary_frequency","log_norm_frequency"
}
  \item{group}{
name of the column to group the data
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns a summary dataframe with average tf.idf for each category and group
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
function (table_N_occurence, weight_method = "brut_frequency", 
    group = NULL) 
{
    if (is.null(group)) {
        dt_weights_years = table_word_weight(table_N_occurence, 
            weight_method) \%>\% ungroup()
        dt_weights_years = dt_weights_years \%>\% gather("Crisis", 
            value = "word_weight")
        dt_weights_years = dt_weights_years[-1, ]
        dt_weights_years = dt_weights_years \%>\% mutate(word_weight = as.numeric(word_weight))
    }
    else {
        dt_weights_years = table_word_weight(table_N_occurence, 
            weight_method) \%>\% ungroup() \%>\% group_by(get(group)) \%>\% 
            summarize_if(is.numeric, mean)
        dt_weights_years = dt_weights_years \%>\% gather("Crisis", 
            value = "word_weight", -"get(group)")
        colnames(dt_weights_years)[1] = group
        dt_weights_years
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
