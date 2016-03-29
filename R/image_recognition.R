#' Detects image recognition
#'
#' Given an input image, returns a series of image classifications and associated scores
#'
#' * Input can be either grayscale or rgb color and should either be a numpy array or nested list format.
#'
#' * Input data should be either uint8 0-255 range values or floating point between 0 and 1.
#'
#' * Large images (i.e. 1024x768+) are much bigger than needed, resizing will be done internally to 64x64 if needed.
#'
#' * For ideal performance, images should be square aspect ratio but non-square aspect ratios are supported as well.
#' @inheritParams face_emotion
#' @return List with image recognition
#' @keywords indico.io machine learning API image recognition recognition
#' @seealso \code{\link{face_emotion}}, \code{\link{facial_features}}
#' @export
#' @import httr rjson stringr
#' @examples
#' img <- "../tests/testthat/image/image.png"
#' features <- image_recognition(img)
#'
#' length(features)
#' min(unlist(features))
#' max(unlist(features))
#' sum(unlist(features))
#'
image_recognition <- function(img, version = NULL, ...) {
  # Checks parameters
  if (missing(img)) {
    stop("No image for analysis provided!")
  }

  img <- format_image(img, 144, TRUE)
  make_request(img, 'imagerecognition', version, ...)
}
