% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/term_frequencies.R
\name{ccdr.tfs.update}
\alias{ccdr.tfs.update}
\title{Updates the tf-indexes without repeating full extraction}
\usage{
ccdr.tfs.update(
  file_tf_to_update,
  corpus_file,
  lexicon = NULL,
  export_file,
  parrallel = T,
  store_old = F,
  store_old_path = NULL
)
}
\arguments{
\item{file_tf_to_update}{Path to old tf dataframe.}

\item{corpus_file}{Path to corpus from which perform the extraction.}

\item{lexicon}{new lexicon of categories}

\item{export_file}{Path to export the file.}

\item{parrallel}{Logical. If TRUE, parallel computation for each category.}

\item{store_old}{Logical. If TRUE, store old extractions in a directory.}

\item{store_old_path}{Path to move old extractions in.}
}
\value{
A dataframe with file name, old indexes not specified in keyword_list and updated indexes.
}
\description{
Combine probability of shocks, intensity and complexity of relations to construct
.a mesure of severity of crisis
}
\author{
Manuel Betin, Umberto Collodel
}
