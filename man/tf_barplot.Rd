\name{tf_barplot}
\alias{tf_barplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
barplot for term frequency
}
\description{
barplot for term frequency
}
\usage{
tf_barplot(tf,vars_type=c("economic_shock","non_economic_shock","debt_outcomes","debt_structure","characteristics_program","adustment_program),vars_nature=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tf}{
term frequency matrix with variables of interest as numeric columns
}
  \item{vars_type}{
Type of the categories to display, a vector of arguments among: "economic_shock","non_economic_shock","debt_outcomes","debt_structure","characteristics_program","adustment_program
}
  \item{vars_nature}{
Nature of the category to display by default NULL but accept a vector of arguments among "exogeneous", "endogeneous","pure_endogeneous"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a list containing the dataframe with the underlying data, a ggplot object dislaying the relative importance of each category, a second ggplot object dislaying the term frequency for each category in percent. 
}
\author{
Manuel Betin
}