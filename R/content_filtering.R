#' Detects NSFW content in an image
#'
#' Given an input image, returns a float probability between 0 and 1 that content is NSFW
#'
#' * Input can be either grayscale or rgb color and should either be a numpy array or nested list format.
#'
#' * Input data should be either uint8 0-255 range values or floating point between 0 and 1.
#'
#' * Large images (i.e. 1024x768+) are much bigger than needed, resizing will be done internally to 64x64 if needed.
#'
#' * For ideal performance, images should be square aspect ratio but non-square aspect ratios are supported as well.
#' @inheritParams face_emotion
#' @return Float with probability of NSFW
#' @keywords indico.io machine learning API image content filtering
#' @seealso \code{\link{face_emotion}}, \code{\link{facial_features}}
#' @export
#' @import httr rjson stringr
#' @examples
#' img <- "../tests/testthat/image/image.png"
#' probabilities <- content_filtering(img)
#'
content_filtering <- function(img, version=NULL, ...) {
  # Checks parameters
  if (missing(img)) {
    stop("No image for analysis provided!")
  }

  img <- format_image(img, 128, TRUE)
  make_request(img, 'contentfiltering', version, ...)
}
