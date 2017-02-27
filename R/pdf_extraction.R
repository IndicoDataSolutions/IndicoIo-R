#' Read and parse PDF files to pull out plain text, images, and metadata
#'
#' Read and parse PDF files to pull out plain text, images, and metadata
#' @inheritParams political
#' @return associative array
#' @pdf_extraction indico.io machine learning API pdf extraction
#' @export
#' @import httr rjson stringr
#' @examples
#' result <- pdf_extractions("http://www.cbu.edu.zm/downloads/pdf-sample.pdf")
#' cat(sprintf("The pdf contained the following text: \n`%s`", result['text']))
#'
pdf_extraction <- function(pdf, version = NULL, ...) {

  # Checks parameters
  if (missing(pdf) || str_trim(pdf) == "") {
    stop("No pdf for analysis provided!")
  }
  preprocessed_pdf <- format_pdf(pdf)
  make_request(preprocessed_pdf, 'pdfextraction', version, ...)
}
