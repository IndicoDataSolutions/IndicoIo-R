#' Returns a float indicating engagement score for the text
#'
#' Given input text, returns a probability distribution over the political
#' alignment of the speaker.
#' @param text text for analysis
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param ... additional arguments to passed to request
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
twitter_engagement <- function(text, api_key = FALSE, cloud = FALSE, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'twitterengagement', api_key, cloud, ...)
}

#' Returns a list of floats indicating engagement scores for the list of text
#'
#' Given input text, returns a probability distribution over the political
#' alignment of the speaker.
#' @param text array of documents for analysis
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param ... additional arguments to passed to request
#' @return List of lists with party probability pairs
#' @keywords indico.io machine learning API twitter engagement analysis
#' @seealso \code{\link{sentiment}}, \code{\link{language}}
#' @export
#' @import httr rjson stringr
#' @examples
#' text_list <- list()
#' text_list[[1]] <- "I am so proud to stand here today
#'            as Prime Minister of four nations
#'            in one United Kingdom."
#' engagements <- batch_twitter_engagement(text_list)
#' cat(sprintf("This text has twitter engagement of %f",
#'             engagements[[1]]))
#'
batch_twitter_engagement <- function(text, api_key = FALSE, cloud = FALSE, ...) {
  make_request(text, 'twitterengagement', api_key, cloud, batch = TRUE, ...)
}
