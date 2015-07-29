TEXT_APIS <- c("sentiment", "text_tags", "political", "language")
IMAGE_APIS <- c("facial_features", "face_emotion", "image_features")
APIS_MAP <- list(
                sentiment="sentiment",
                text_tags="texttags",
                political="political",
                language="language",
                facial_features="facialfeatures",
                face_emotion="fer",
                image_features="imagefeatures"
            )

#' Checks if api is allowed and maps it to the REST Api name
#'
#' Given vector of input apis and vector of accepted APIs, returns vector of mapped apis
#' @param input vector of string api names
#' @param allowed vector of acceptable string api names
#' @return vector of converted api names
filter_map <- function(input, allowed) {
    lapply(input, function (api) {
        if (!is.element(api, allowed)) {
            stop(str_c("Given API is not allowed! Please use ", paste(allowed, collapse=", ")))
        }

        APIS_MAP[[api]]
    });
}
#' Converts response list to one that uses user inputted api names
#'
#' Given response list and list of user input apis, return properly named response
#' @param results response list
#' @param apis vector of provided string api names
#' @return converted_results converted response list
convert_results <- function(results, apis) {
    converted_results <- list()
    for (api in apis) {
        response <- results[[APIS_MAP[[api]]]];
        if ("results" %in% names(response)) {
            converted_results[[api]] <- response[["results"]]
        } else {
            converted_results[[api]] <- response[["error"]]
        }
    }

    converted_results
}

#' Returns multiple text API results in a single request / response
#'
#' Given input text and a vector of API names, returns aggregated results of the apis.
#' @param text text for analysis
#' @param apis vector / array of API names as strings
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param ... additional arguments to passed to request
#' @return map of apis as keys to their results as values
#' @keywords indico.io machine learning API multi API analysis
#' @export
#' @import httr rjson stringr
#' @examples
#' results <- predict_text("Thanks everyone for the birthday wishes!!
#'                       It was a crazy few days ><", apis=c("sentiment", "political"))
#' results
#' cat(sprintf("This text has %s tonality",
#'              ifelse(results[["sentiment"]] > 0.5, "positive", "negative")))
#'
predict_text <- function(text, apis = c("sentiment", "text_tags", "political", "language"), api_key = FALSE, cloud = FALSE, ...) {
    # Checks parameters
    if (missing(text) || str_trim(text) == "") {
        stop("No text for analysis provided!")
    }
    converted_apis <- filter_map(apis, TEXT_APIS)

    results <- make_request(text, "apis", api_key, cloud, apis=converted_apis, ...)
    results <- convert_results(results, apis)
}


#' Returns multiple image API results in a single request / response
#'
#' Given input text and a vector of API names, returns aggregated results of the apis.
#' @param img image data
#' @param apis vector / array of API names as strings
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param ... additional arguments to passed to request
#' @return map of apis as keys to their results as values
#' @keywords indico.io machine learning API multi API analysis
#' @export
#' @import httr rjson stringr
#' @examples
#' ## Example 1
#' img <- "../tests/testthat/image/image.png"
#' emotion <- predict_image(img, apis=c("face_emotion"))
#'
predict_image <- function(img, apis = c("facial_features", "face_emotion", "image_features"), api_key = FALSE, cloud = FALSE, ...) {
    # Checks parameters
    if (missing(img)) {
      stop("No image for analysis provided!")
    }

    converted_apis <- filter_map(apis, IMAGE_APIS)
    img <- format_image(img, 128)
    results <- make_request(img, "apis", api_key, cloud, apis=converted_apis, ...)
    results <- convert_results(results, apis)
}
