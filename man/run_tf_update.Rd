\name{run_tf_update}
\alias{run_tf_update}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function that update the tf  output of run_tf with the new variables, avoiding recomputing all the categories
}
\description{
function that update the tf  output of run_tf with the new variables, avoiding recomputing all the categories
}
\usage{
run_tf_update(path_tf_to_update = paste0(root_path, "/3. Data/IMF Letters of Intents/data_LoI_N_Occ_crisis_words.RData"), corpus_path = paste0(root_path, "/3. Data/IMF Letters of Intents/IMF_letofIntent_1960_2014_clean.RData"), type_lexicon = "words", keyword_list = NULL, export_path = paste0(root_path, "/3. Data/IMF Letters of Intents/data_LoI_N_Occ_crisis_words.RData"), parrallel = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_tf_to_update}{
path_tf_to_update_path: path where to locate the RData file containing the tf dataframe to update
}
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

## The function is currently defined as
function (path_tf_to_update = paste0(root_path, "/3. Data/IMF Letters of Intents/data_LoI_N_Occ_crisis_words.RData"), 
    corpus_path = paste0(root_path, "/3. Data/IMF Letters of Intents/IMF_letofIntent_1960_2014_clean.RData"), 
    type_lexicon = "words", keyword_list = NULL, export_path = paste0(root_path, 
        "/3. Data/IMF Letters of Intents/data_LoI_N_Occ_crisis_words.RData"), 
    parrallel = T) 
{
    if (is.null(keyword_list)) {
        print("Updating all columns")
        new_tf = run_tf(corpus_path = corpus_path, type_lexicon = type_lexicon, 
            keyword_list = key_words_crisis(), export_path = paste0(root_path, 
                "/3. Data/IMF Letters of Intents/data_LoI_N_Occ_crisis_words.RData"), 
            parrallel = parrallel)
        return(new_tf)
    }
    else {
        print("updating selected columns")
        tf_to_update = rio::import(path_tf_to_update)
        dim_tf_to_update = dim(tf_to_update)
        existing_cols = names(tf_to_update)
        if (any(existing_cols \%in\% keyword_list)) {
            tf_to_update = tf_to_update \%>\% dplyr::select(-keyword_list)
        }
        corpus = rio::import(corpus_path)
        new_tf = run_tf(corpus_path = corpus_path, type_lexicon = type_lexicon, 
            keyword_list = keyword_list, parrallel = parrallel)
        tf_to_update = left_join(x = tf_to_update, y = new_tf, 
            by = "file")
        print(paste0("Non updated columns:\n\n                 ", 
            paste0(existing_cols, collapse = ", ")))
        print(paste0("Updated columns:\n\n                 ", 
            paste0(keyword_list, collapse = ", ")))
        rio::export(tf_to_update, export_path)
        return(tf_to_update)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
