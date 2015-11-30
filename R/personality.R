#' Predicts a score between 0 and 1 for 4 personality traits for the author of the texts
#'
#' Predicts a score between 0 and 1 for 4 personality traits for the author of the texts.
#' The personality scales are "openness", "extraversion", "conscientiousness", and "agreeableness"
#' @inheritParams political
#' @return dict with likelihood the author would be catigorized as each myers briggs axis,
#' and the most likley MB type
#' @personality indico.io machine learning API personality tagging
#' @seealso \code{\link{political}}, \code{\link{sentiment}}, \code{\link{text_tags}}
#' @export
#' @import httr rjson stringr
#' @examples
#' personality <- personality("Monday: Delightful with mostly sunny skies.
#'                    Highs in the low 70s.")
#' cat(sprintf("The author of text is %s",
#'              ifelse(personality['extraversion'] < .5, "introverted", "extraverted")))
#'
personality <- function(text,  api_key = FALSE, cloud = FALSE, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'personality', api_key, cloud, version, ...)
}

#'@export
batch_personality <- function(text, ...) {
    warning("The `batch_personality` function will be deprecated in the next major upgrade. " +
      "Please call `personality` instead with the same arguments")
    personality(text, ...)
}
