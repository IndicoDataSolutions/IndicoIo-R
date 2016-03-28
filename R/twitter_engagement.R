#' Returns a float indicating engagement score for the text
#'
#' Given input text, returns a probability distribution over the political
#' alignment of the speaker.
#' @inheritParams political
#' @return float engagement score
#' @keywords indico.io machine learning API twitter engagement analysis
#' @seealso \code{\link{sentiment}}, \code{\link{language}}
#' @export
#' @import httr rjson stringr
#' @examples
#' text <- "I am so proud to stand here today
#'            as Prime Minister of four nations
#'            in one United Kingdom."
#' engagement <- twitter_engagement(text)
#' cat(sprintf("This text has twitter engagement of %f",
#'             engagement))
#'
twitter_engagement <- function(text, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'twitterengagement', version, ...)
}
