TEXT_APIS <- c("sentiment", "text_tags", "political", "language", "twitter_engagement", "keywords", "named_entities", "personality")
IMAGE_APIS <- c("facial_features", "fer", "image_features", "content_filtering", "facial_localization")
APIS_MAP <- list(
    sentiment="sentiment",
    text_tags="texttags",
    political="political",
    language="language",
    facial_features="facialfeatures",
    facial_localization="faciallocalization",
    face_emotion="fer",
    fer="fer",
    image_features="imagefeatures",
    twitter_engagement="twitterengagement",
    keywords="keywords",
    named_entities="namedentities",
    content_filtering="contentfiltering",
    personality="personality"
)

api_types <- function() {
    L = list()
    for (api in TEXT_APIS) {
        L[[api]] <- "text"
    }
    for (api in IMAGE_APIS) {
        L[[api]] <- "image"
    }
    L
}

API_TYPES = api_types()

filter_apis <- function(input, allowed) {
    lapply(input, function (api) {
        if (!is.element(api, allowed)) {
            stop(str_c("Given API is not allowed! Please use ", paste(allowed, collapse=", ")))
        }
    });
    input
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
        response <- results[[api]];
        if ("results" %in% names(response)) {
            converted_results[[api]] <- response[["results"]]
        } else {
            converted_results[[api]] <- response[["error"]]
        }
    }
    converted_results
}


#' Returns strength of correlation between API results
#'
#' Given an array of data and a vector of apis, returns a nested associate array of correlation strengths
#' @param data input data
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param apis vector of provided string api names
#' @param version - api version
#' @param ... additional arguments to passed to request
#' @return nested associated array of correlation strengths
intersections <- function(data, apis=NULL, version = NULL, ...) {

    if (length(apis) != 2) {
        stop("Argument 'apis' must be of length 2")
    }
    if (!(typeof(data) == "list") && length(data) < 3) {
        stop("At least 3 examples are required to use the intersections API.")
    }
    if (API_TYPES[[apis[1]]] != API_TYPES[[apis[2]]]) {
        stop("Both 'apis' must accept the same kind of input to use the intersections API")
    }

    results <- make_request(data, "apis/intersections", version=version, apis=apis, ...)
}



#' Returns multiple text API results in a single request / response
#'
#' Given input text and a vector of API names, returns aggregated results of the apis.
#' @inheritParams political
#' @param apis vector / array of API names as strings
#' @return map of apis as keys to their results as values
#' @keywords indico.io machine learning API multi API analysis
#' @export
#' @import httr rjson stringr
#' @examples
#' results <- analyze_text("Thanks everyone for the birthday wishes!!
#'                       It was a crazy few days ><", apis=c("sentiment", "political"))
#' results
#' cat(sprintf("This text has %s tonality",
#'              ifelse(results[["sentiment"]] > 0.5, "positive", "negative")))
#'
analyze_text <- function(text, apis = c("sentiment", "text_tags", "political", "language", "keywords", "twitter_engagement", "named_entities"), version = NULL, ...) {
    # Checks parameters
    if (missing(text) || str_trim(text) == "") {
        stop("No text for analysis provided!")
    }
    apis <- filter_apis(apis, TEXT_APIS)

    results <- make_request(text, "apis", version=version, apis=apis, ...)
    results <- convert_results(results, apis)
}


#' Returns multiple image API results in a single request / response
#'
#' Given input text and a vector of API names, returns aggregated results of the apis.
#' @inheritParams face_emotion
#' @param apis vector / array of API names as strings
#' @return map of apis as keys to their results as values
#' @keywords indico.io machine learning API multi API analysis
#' @export
#' @import httr rjson stringr
#' @examples
#' ## Example 1
#' img <- "../tests/testthat/image/image.png"
#' emotion <- analyze_image(img, apis=c("fer"))
#'
analyze_image <- function(img, apis = c("facial_features", "fer", "image_features", "facial_localization", "content_filtering"), version = NULL, ...) {
    # Checks parameters
    if (missing(img)) {
      stop("No image for analysis provided!")
    }

    apis <- filter_apis(apis, IMAGE_APIS)
    img <- format_image(img, 128)
    results <- make_request(img, "apis", version=version, apis=apis, ...)
    results <- convert_results(results, apis)
}
