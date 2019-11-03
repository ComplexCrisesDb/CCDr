\name{Find_targeted_pages}
\alias{Find_targeted_pages}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 provide files either pdf of html and return the paragraphs matching the targetted word
}
\description{
 provide files either pdf of html and return the paragraphs matching the targetted word
}
\usage{
Find_targeted_pages(file, targetword)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
a string of characters where to perform the search
}
  \item{targetword}{
the target word to locate in the file
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function return the target word provided, the number of occurence by page, the total occurence in the document and the
pages of the document that contain the words 
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
function (file, targetword) 
{
    require(stringi)
    require(stringr)
    page_locations = list()
    target_pages = list()
    i = 0
    Tot.occurence = 0
    file = clean_text(file)
    n.chars = sum(nchar(file))
    page_location = sapply(1:length(file), function(x) {
        page = file[[x]]
        page = tibble(page) \%>\% unnest_tokens(word, page, token = "ngrams", 
            n = 3) \%>\% mutate(word = tolower(word))
        n.occurence = (page \%>\% filter(grepl(paste(tolower(targetword), 
            collapse = "|"), word)) \%>\% summarize(count = n()))$count
        condition = n.occurence > 0
        if (any(condition)) {
            i <<- i + 1
            page_locations[[i]] <<- paste0("Found in page ", 
                x, " :", n.occurence, " times")
            target_pages[[i]] <<- file[[x]]
            Tot.occurence <<- Tot.occurence + n.occurence
            return("yes")
        }
        else {
            return("no")
        }
    })
    return(list(target = targetword, N.chars = n.chars, N.Occurence = page_locations, 
        Tot.occurence = Tot.occurence, pages = target_pages))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
