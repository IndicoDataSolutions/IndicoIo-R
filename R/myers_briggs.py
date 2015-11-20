#' Tags text likely myers briggs types
#'
#' Given input text, returns the likelihood the author would be catigorized as E vs I,
#' S vs N, T vs F, and J vs P Also returns the most likely Myers Briggs
#' personality type under the key 'personality'.
#' @inheritParams political
#' @return dict with likelihood the author would be catigorized as each myers briggs axis,
#' and the most likley MB type
#' @myers_briggs indico.io machine learning API personality tagging
#' @seealso \code{\link{political}}, \code{\link{sentiment}}, \code{\link{text_tags}}
#' @export
#' @import httr rjson stringr
#' @examples
#' myers_briggs <- myers_briggs("Monday: Delightful with mostly sunny skies.
#'                    Highs in the low 70s.")
#' cat(sprintf("The author of text is %s",
#'              ifelse(myers_briggs['I'] > myers_briggs['E'], "introverted", "extraverted")))
#' cat(sprintf("The author of text is most likely the %s personality type", myers_briggs['personality'])
#'
myers_briggs <- function(text,  api_key = FALSE, cloud = FALSE, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'myersbriggs', api_key, cloud, version, ...)
}

#'@export
batch_myers_briggs <- function(text, ...) {
    warning("The `batch_myers_briggs` function will be deprecated in the next major upgrade. " +
      "Please call `myers_briggs` instead with the same arguments")
    keywords(text, ...)
}
