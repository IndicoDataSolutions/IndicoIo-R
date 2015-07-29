#' Detects facial features
#'
#' Given an grayscale input image of a face, returns a 48 dimensional feature vector explaining that face.
#' Useful as a form of feature engineering for face oriented tasks.
#' Input should be in a list of list format, resizing will be attempted internally but for best
#' performance, images should be already sized at 48x48 pixels.
#' @inheritParams face_emotion
#' @return List with face features
#' @keywords indico.io machine learning API face features recognition
#' @seealso \code{\link{face_emotion}}, \code{\link{image_features}}
#' @export
#' @import httr rjson stringr
#' @examples
#' img <- "../tests/testthat/image/image.png"
#' features <- facial_features(img)
#' length(features)
#'
facial_features <- function(img, api_key = FALSE, cloud = FALSE, ...) {
  # Checks parameters
  if (missing(img)) {
    stop("No image for analysis provided!")
  }

  img <- format_image(img, 48)
  make_request(img, 'facialfeatures', api_key, cloud, ...)
}

face_features <- facial_features
