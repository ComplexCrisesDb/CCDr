\name{idf_barplot}
\alias{idf_barplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
barplot for the idf
}
\description{
barplot for the idf
}
\usage{
idf_barplot(idf,vars_type=c("economic_shock","non_economic_shock","debt_outcomes","debt_structure","characteristics_program","adustment_program"),vars_nature=NULL,idf_trans=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{idf}{
  Inverse document frequency dataframe (output of idf())
%%     ~~Describe \code{x} here~~
}
  \item{vars_type}{
Type of the categories to display, a vector of arguments among: "economic_shock","non_economic_shock","debt_outcomes","debt_structure","characteristics_program","adustment_program
}
  \item{vars_nature}{
Nature of the category to display by default NULL but accept a vector of arguments among "exogeneous", "endogeneous","pure_endogeneous"
}
  \item{idf_trans}{
T/F, F to display the idf, T to display the proportion of documents.
}
}
\author{
Manuel Bétin
}