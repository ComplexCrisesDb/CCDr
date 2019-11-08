\name{ngram_clean}
\alias{ngram_clean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
generate tibble with ngrams of specified length in rows and dummies for  a set of typologies, the function clean the text by removing unnecessary characters and removing common stop words.

}
\description{
generate tibble with ngrams of specified length in rows and dummies for  a set of typologies, the function clean the text by removing unnecessary characters and removing common stop words.

}
\usage{
ngram_clean(text, length_ngram = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
a character string containing the text to analyse
}
  \item{length_ngram}{
the number of word to consider has the unit of observation
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a tibble with ngrams in rows, number of occurence and dummies for the typologies 
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
function (text, length_ngram = 4) 
{
    text = text \%>\% clean_text() \%>\% tibble()
    text = text \%>\% unnest_tokens(ngram, text, token = "ngrams", 
        n = length_ngram)
    list_nwords = paste("word", 1:length_ngram, sep = "")
    ngrams_sep = text \%>\% dplyr::select(ngram) \%>\% separate(ngram, 
        list_nwords, sep = " ")
    ngrams_sep = ngrams_sep \%>\% mutate_at(vars(matches("word")), 
        remove_stop) \%>\% unite("word", list_nwords, sep = " ") \%>\% 
        dplyr::select(word) \%>\% mutate(word = str_trim(word), 
        word = str_replace(word, "  ", " ")) \%>\% filter(word != 
        "")
    ngrams_sep = ngrams_sep \%>\% filter(!str_detect(word, "\\d") | 
        !str_detect(word, "perce"))
    ngrams_sep = ngrams_sep \%>\% count(word, sort = T)
    ngrams_sep = ngrams_sep \%>\% mutate(crisis = ifelse(str_detect(word, 
        "crisis") | str_detect(word, "distress") | str_detect(word, 
        "downturn"), 1, 0), law = ifelse(str_detect(word, "law") | 
        str_detect(word, "legal") | str_detect(word, "regulation") | 
        str_detect(word, "supervision"), 1, 0), finance = ifelse(str_detect(word, 
        "finance"), 1, 0), supervision = ifelse(str_detect(word, 
        "supervis"), 1, 0), solvency = ifelse(str_detect(word, 
        "solven"), 1, 0), liquidity = ifelse(str_detect(word, 
        "liquid"), 1, 0), budget = ifelse(str_detect(word, "budget"), 
        1, 0), guarantee = ifelse(str_detect(word, "guarantee"), 
        1, 0), nationalization = ifelse(str_detect(word, "nationalization"), 
        1, 0), overdue = ifelse(str_detect(word, "overdue"), 
        1, 0), accomodative = ifelse(str_detect(word, "accomodative"), 
        1, 0), administrative = ifelse(str_detect(word, "administrative"), 
        1, 0), economic = ifelse(str_detect(word, "economic"), 
        1, 0), money = ifelse(str_detect(word, "money"), 1, 0), 
        arrears = ifelse(str_detect(word, "arrears"), 1, 0), 
        exchange = ifelse(str_detect(word, "exchange"), 1, 0), 
        depreciation = ifelse(str_detect(word, "depreciation"), 
            1, 0), banks = ifelse(str_detect(word, "bank"), 1, 
            0), capital = ifelse(str_detect(word, "capital"), 
            1, 0), asset = ifelse(str_detect(word, "asset"), 
            1, 0), reform = ifelse(str_detect(word, "reform"), 
            1, 0), debt = ifelse(str_detect(word, "debt"), 1, 
            0), loans = ifelse(str_detect(word, "loan") | str_detect(word, 
            "portfolio") | str_detect(word, "lend"), 1, 0))
    ngrams_sep = ngrams_sep \%>\% filter(crisis == 1 | law == 1 | 
        finance == 1 | supervision == 1 | solvency == 1 | liquidity == 
        1 | budget == 1 | guarantee == 1 | nationalization == 
        1 | overdue == 1 | accomodative == 1 | administrative == 
        1 | economic == 1 | money == 1 | arrears == 1 | exchange == 
        1 | depreciation == 1 | banks == 1 | capital == 1 | asset == 
        1 | reform == 1 | debt == 1 | loans == 1)
    return(ngrams_sep)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
