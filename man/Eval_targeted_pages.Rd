\name{Eval_targeted_pages}
\alias{Eval_targeted_pages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
provide list of files and return summary of counts of occurence of the target world 
}
\description{
provide list of files and return summary of counts of occurence of the target world 
}
\usage{
Eval_targeted_pages(files, targetword, brute_freq = F, parrallel = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{
a list of string of characters where to perform the search
}
  \item{targetword}{
the target word to locate in the file
}
  \item{brute_freq}{
the type of frequency to choose
}
  \item{parrallel}{
T/F whether to use mcparrallel to perform parrallel computing (only available on mac/linux)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function return the number of occurence for each file
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
function (files, targetword, brute_freq = F, parrallel = T) 
{
    if (parrallel) {
        metric = parallel::mclapply(files, function(x) {
            file = Find_targeted_pages(x, targetword)
            if (brute_freq) {
                file$Tot.occurence
            }
            else {
                file$Tot.occurence/file$N.chars
            }
        }, mc.cores = parallel::detectCores() - 1, mc.allow.recursive = TRUE)
        N.Occurence = do.call(rbind, metric)
        if (length(targetword) == 1) {
            colnames(N.Occurence) = targetword
        }
        else colnames(N.Occurence) = "Occurence"
    }
    else {
        metric = lapply(files, function(x) {
            file = Find_targeted_pages(x, targetword)
            if (brute_freq) {
                file$Tot.occurence
            }
            else {
                file$Tot.occurence/file$N.chars
            }
        })
        N.Occurence = do.call(rbind, metric)
        if (length(targetword) == 1) {
            colnames(N.Occurence) = targetword
        }
        else colnames(N.Occurence) = "Occurence"
    }
    return(N.Occurence)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
