#' Given an article or group of articles, identifies key summary sentences.
#'
#' Finds summary sentences in input documents.
#' @inheritParams political
#' @return list of strings
#' @summarization indico.io machine learning API summarization
#' @seealso \code{\link{text_features}}
#' @export
#' @import httr rjson stringr
#' @examples
#' result <- summarization("One sentence. An additional sentence. Too much text to read.", top_n = 1)
#' cat(sprintf("`%s` was identified as a summary sentence.", result['text']))
#'
summarization <- function(text, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'summarization', version, ...)
}
