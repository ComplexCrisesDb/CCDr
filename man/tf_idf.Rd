\name{tf_idf}
\alias{tf_idf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
function to compute the weights of each word by doing the product of the index of frequency and the inverse
  document frequency
}
\description{
function to compute the weights of each word by doing the product of the index of frequency and the inverse
  document frequency
}
\usage{
tf_idf(table_N_occurence, weight_method = "brut_frequency")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_N_occurence}{
output of table_brute_frequency_vector()
}
  \item{weight_method}{
one of "brut_frequency", "binary_frequency","log_norm_frequency"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the function returns a dataframe with categories in rows and a column with the tf.idf
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
function (table_N_occurence, weight_method = "brut_frequency") 
{
    dt_inv_doc_freq = try(idf(table_N_occurence))
    if ("try_error" \%in\% dt_inv_doc_freq) {
        print(paste0("Warning: error when using function dt_inv_doc_freq"))
        return(NULL)
    }
    select_cols = names(table_N_occurence \%>\% dplyr::select_if(is.numeric))
    if (weight_method == "brut_frequency") {
        dt_words_weight = table_N_occurence
        for (var in select_cols) {
            dt_words_weight[, var] = dt_words_weight[, var] * 
                (dt_inv_doc_freq \%>\% filter(Crisis == var) \%>\% 
                  dplyr::select(idf))[[1]]
        }
        return(dt_words_weight)
    }
    else if (weight_method == "binary_frequency") {
        dt_words_weight = binary_frequency_trans(table_N_occurence)
        for (var in select_cols) {
            dt_words_weight[, var] = dt_words_weight[, var] * 
                (dt_inv_doc_freq \%>\% filter(Crisis == var) \%>\% 
                  dplyr::select(idf))[[1]]
        }
        return(dt_words_weight)
    }
    else if (weight_method == "log_norm_frequency") {
        dt_words_weight = log_norm_trans(table_N_occurence)
        for (var in select_cols) {
            dt_words_weight[, var] = dt_words_weight[, var] * 
                (dt_inv_doc_freq \%>\% filter(Crisis == var) \%>\% 
                  dplyr::select(idf))[[1]]
        }
        return(dt_words_weight)
    }
    else warning("please choose a proper method: brut_frequency,binary_frequency,log_norm_frequency")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
