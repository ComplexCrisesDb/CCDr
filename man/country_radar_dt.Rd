\name{country_radar_dt}
\alias{country_radar_dt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Look at average importance of each crisis by country and display the more important elements
}
\description{
Look at average importance of each crisis by country and display the more important elements
}
\usage{
country_radar_dt(table_N_occurence, isoc, top_n = 50, weight_method = "brut_frequency", group = "ISO3_Code")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table_N_occurence}{
tf.idf dataframe (output of table_word_weight())
}
  \item{isoc}{
the iso code of the country of interest
}
  \item{top_n}{
the maximum number of categories to display
}
  \item{weight_method}{
one of "brut_frequency", "binary_frequency","log_norm_frequency"
}
  \item{group}{
grouping variable
}
}
\value{
the function returns a dataframe
}

\author{
Manuel Betin
}