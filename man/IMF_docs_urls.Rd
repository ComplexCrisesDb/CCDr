\name{IMF_docs_urls}
\alias{IMF_docs_urls}
\docType{data}
\title{
dataset containing the urls of the documents extracted from IMF archives
}
\description{
The database containing the urls for IMF documents
}
\usage{data("IMF_docs_urls")}
\format{
  A data frame with 0 observations on the following 2 variables.
  \describe{
    \item{\code{ID}}{ISO 3 code of the country}
    \item{\code{period}}{date of the release of the document in date format}
    \item{\code{title}}{title as provided in the metadata during the extraction}
    \item{\code{hierarchy}}{The IMF code document if available}
    \item{\code{pdf}}{the url to access the document}
    \item{\code{type_doc_program}}{type of document of lending arrangement (request/Reviews/modifications...)}
    \item{\code{type_program}}{type of document lending arrangement (SBA, EFF,...)}
    \item{\code{type_doc_consultations}}{type of document of consultations}
    \item{\code{Review_number}}{for reviews of lending arrangement the number of the review)}
    \item{\code{perf_criteria}}{dummy equal to 1 if mention of performance criteria in the title)}
    \item{\code{waiver}}{dummy equal to 1 if mention of waiver in the title)}
    \item{\code{waiver}}{dummy equal to 1 if mention of membership in the title)}
    \item{\code{waiver}}{dummy equal to 1 if mention of statement in the title)}
    \item{\code{repurchase_transaction}}{dummy equal to 1 if mention of repurchase transaction in the title)}
    \item{\code{technical_assistance}}{dummy equal to 1 if mention of technical assistance in the title)}
    \item{\code{expost_assistance}}{dummy equal to 1 if mention of expost assistance in the title)}
    \item{\code{exchange_system}}{dummy equal to 1 if mention of exchange system in the title)}
    \item{\code{overdue_obligations}}{dummy equal to 1 if mention of overdue obligations in the title)}
    \item{\code{type_hierarchy}}{whether the document is a correction, a supplement, or the original file)}
  }
}
\examples{
data(IMF_docs_urls)
## maybe str(IMF_docs_urls) ; plot(IMF_docs_urls) ...
}
\keyword{datasets}
