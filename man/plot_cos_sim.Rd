\name{plot_cos_sim}
\alias{plot_cos_sim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute the cosinus similarity matrix by comparing pairwise columns
}
\description{
compute the cosinus similarity matrix by comparing pairwise columns
}
\usage{
plot_cos_sim(cos_sim, var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cos_sim}{
cosinus similarity matrix output of matrix_cosinus_similarity()
}
  \item{var}{
the name of the category to plot
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
plot results of matric cosinus for one selected column 
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
function (cos_sim, var) 
{
    dt = cos_sim \%>\% dplyr::select(myvar = var)
    dt$xaxis = rownames(dt)
    dt = dt \%>\% mutate(xaxis = str_replace(xaxis, "_", " "))
    ggplot(data = dt) + geom_bar(stat = "identity", aes(x = reorder(xaxis, 
        -myvar), y = myvar)) + theme_bw() + ylab("Cosinus similarity") + 
        xlab("") + theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, 
        hjust = 1), axis.text = element_text(size = 8))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
