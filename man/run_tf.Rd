\name{run_tf}
\alias{run_tf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function that compute the tf matrix for a corpus given in a list format, the type of lexicon to use and the sublist of keywords associated
}
\description{
function that compute the tf matrix for a corpus given in a list format, the type of lexicon to use and the sublist of keywords associated
}
\usage{
run_tf(corpus_path = paste0(root_path, "/3. Data/IMF Letters of Intents/IMF_letofIntent_1960_2014_clean.RData"), type_lexicon = "words", keyword_list = c("Commodity_crisis", "Balance_payment_crisis", "Inflation_crisis"), export_path = NULL, parrallel = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus_path}{
path where to locate the RData file containing the list of documents (in string of characters)
}
  \item{type_lexicon}{
one of "words" or "categories"
}
  \item{keyword_list}{
the names of the categories (in key_words_crisis() or key_words_categories())
}
  \item{export_path}{
where to export the resulting dataframe
}
  \item{parrallel}{
T/F to use mcparralle (only available on mac/linux)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The output is a matrix of tf with a row per document and a column for each element of the keyword list 
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
function (corpus_path = paste0(root_path, "/3. Data/IMF Letters of Intents/IMF_letofIntent_1960_2014_clean.RData"), 
    type_lexicon = "words", keyword_list = c("Commodity_crisis", 
        "Balance_payment_crisis", "Inflation_crisis"), export_path = NULL, 
    parrallel = T) 
{
    print(paste0("Loading corpus from ", corpus_path))
    corpus = rio::import(corpus_path)
    if (type_lexicon == "words") {
        if (is.null(keyword_list)) {
            keyword_list = c("Commodity_crisis", "Balance_payment_crisis", 
                "Inflation_crisis", "World_outcomes", "Floating_Exchange rate", 
                "Fixed_Exchange rate", "Wars", "Soft_Recession", 
                "Fiscal_Outcomes", "Financial_crisis", "Banking_crisis", 
                "Currency_crisis", "Severe_recession", "Crisis_contagion", 
                "Sovereign_default", "Expectations", "Precautionary_programs", 
                "Natural_desaster", "Loss_Market_access", "Political_crisis", 
                "Constraining", "Contagion")
            print(keyword_list)
        }
        keyword_list = key_words_crisis()[keyword_list]
    }
    else if (type_lexicon == "category") {
        if (is.null(keyword_list)) {
            keyword_list = c("Exogenous", "Manifestations", "Instruments")
        }
        keyword_list = key_words_categories()[keyword_list]
    }
    else {
        print("Please provide a valid type_lexicon, either 'words' or 'category'")
        return(NULL)
    }
    tic()
    dt = table_brute_frequency_vector(corpus, keyword_list, parrallel = parrallel)
    toc()
    destination = paste0(export_path, "/data_LoI_N_Occ_crisis_", 
        type_lexicon, ".RData")
    print(paste0("export table in ", corpus_path))
    if (!is.null(export_path)) {
        rio::export(dt, destination)
    }
    dt
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
