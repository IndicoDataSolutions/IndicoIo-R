Collection <- setClass(
    "Collection",

    slots = c(
              name = "character",
              domain = "ANY"
             ),
             # Set the default values for the slots. (optional)
   prototype=list(
           name="custom_collection",
           domain=NULL
           ),
    )

setGeneric(name="addData",
           def=function(collection_object, data, api_key = FALSE, cloud = FALSE, version = NULL, domain = NULL, ...) {
              standardGeneric("addData")
           }
           )

#' This is the basic training endpoint. Given a piece of text and a score, either categorical
#' or numeric, this endpoint will train a new model given the additional piece of information.
#'
#' @param collection the collection object for this model
#' @param data the text and collection/score associated with it. The length of the text (string) should ideally
#' be longer than 100 characters and contain at least 10 words. While the API will support
#' shorter text, you will find that the accuracy of results improves significantly with longer
#' examples. For an additional fee, this end point will support image input as well. The collection/score
#' can be a string or float. This is the variable associated with the text. This can either be categorical
#' (the tag associated with the post) or numeric (the number of Facebook shares the post
#' received). However it can only be one or another within a given label.
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param version for api version
#' @param ... additional arguments to passed to request
#' @return List with available collections
#' @export
#' @import httr rjson stringr
#' @examples
#' collection <- Collection(name='example')
#' test_data <- list(list("I love my friends!", "extrovert"),
#'                   list("I love to be alone", "introvert"),
#'                   list("I have mixed feelings on people", "ambivert"))
#' addData(collection, test_data)
setMethod(f="addData",
          signature="Collection",
          definition=function(collection_object, data, api_key = FALSE, cloud = FALSE, version = NULL, domain = NULL, ...) {
                collection_object@domain <- ifelse(domain!=NULL, domain, collection_object@domain)
                batch <- typeof(data[[1]]) == "list" || length(data[[1]]) > 1
                if (batch) {
                    image_process <- function(data_pair) {
                        data_pair[1] = format_image(data_pair[[1]], 48)
                        data_pair
                    }
                    data = lapply(data, image_process)
                } else {
                    data[1] = format_image(data[[1]], 48)
                }
                make_request(data, 'custom', api_key, cloud, version, collection = collection_object@name, method = "add_data", domain=collection_object@domain, ...)
          }
          )

setGeneric(name="clear",
           def=function(collection_object, data, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
              standardGeneric("clear")
           }
           )

#' This is an API made to remove all of the data associated from a given colletion. If there's been a data
#' corruption issue, or a large amount of incorrect data has been fed into the API it is often difficult
#' to correct. This allows you to clear a colletion and start from scratch. Use with caution! This is not
#' reversible.
#'
#' @param collection the collection object for this model
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param version for api version
#' @param ... additional arguments to passed to request
#' @return List with available collections
#' @export
#' @import httr rjson stringr
#' @examples
#' collection <- Collection(name='example')
#' clear(collection)
setMethod(f="clear",
          signature="Collection",
          definition=function(collection_object, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
              make_request(NULL, 'custom', api_key, cloud, version, collection = collection_object@name, method = "clear_collection", ...)
          }
          )

setGeneric(name="train",
           def=function(collection_object, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
              standardGeneric("train")
           }
           )

#' This is the basic training endpoint. Given an existing dataset this endpoint will train a model.
#'
#' @param collection the collection object for this model
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param version for api version
#' @param ... additional arguments to passed to request
#' @return List with available collections
#' @export
#' @import httr rjson stringr
#' @examples
#' collection <- Collection(name='example')
#' train(collection)
setMethod(f="train",
          signature="Collection",
          definition=function(collection_object, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
              make_request(NULL, 'custom', api_key, cloud, version, collection = collection_object@name, method = "train", ...)
          }
          )

setGeneric(name="info",
           def=function(collection_object, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
              standardGeneric("info")
           }
           )

#' Return the current state of the model associated with a given collection
#'
#' @param collection the collection object for this model
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param version for api version
#' @param ... additional arguments to passed to request
#' @return List with available collections
#' @export
#' @import httr rjson stringr
#' @examples
#' collection <- Collection(name='example')
#' status = info(collection)
#' cat(sprintf("This collection is a %s model trained on %s data with %i examples",
#'             status[[model_type]],
#'             status[[input_type]],
#'             status[[number_of_examples]])
setMethod(f="info",
          signature="Collection",
          definition=function(collection_object, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
              collections <- collections(api_key, cloud, version, ...)
              collections[[collection_object@name]]
          }
          )

setGeneric(name="wait",
           def=function(collection_object, interval = 1, timeout=60, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
              standardGeneric("wait")
           }
           )

#' Block until the collection's model is completed training
#'
#' @param collection the collection object for this model
#' @param interval how reguarly to check if the model is done training
#' @param timeout max time to wait before erroring
#' @param collection the collection object for this model
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param version for api version
#' @param ... additional arguments to passed to request
#' @return List with available collections
#' @export
#' @import httr rjson stringr
#' @examples
#' collection <- Collection(name='example')
#' test_data <- list(list("I love my friends!", "extrovert"),
#'                   list("I love to be alone", "introvert"),
#'                   list("I have mixed feelings on people", "ambivert"))
#' addData(collection, test_data)
#' train(collection)
#' wait(collection)
setMethod(f="wait",
          signature="Collection",
          definition=function(collection_object, interval = 1, timeout=60, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
            for (i in 1:ceiling(timeout/interval)) {
              status <- info(collection_object, api_key, cloud, version, ...)[['status']]
              if (status == "ready") {
                  return(TRUE)
              }
              if (status != "training") {
                  stop(collection_object@name + " failed with error: " + status)
                  return(FALSE)
              }
              Sys.sleep(interval)
            }
            stop('Timeout error in wait')
          }
          )

setGeneric(name="predict",
           def=function(collection_object, data, api_key = FALSE, cloud = FALSE, version = NULL, domain = NULL, ...) {
              standardGeneric("predict")
           }
           )
#'  This is the prediction endpoint. This will be the primary interaction point for all predictive
#'  analysis.
#'
#' @param collection the collection object for this model
#' the response format for the given label will match the format of the training examples
#' @param data the text example being provided to the API. As a general rule, the data should be as
#' similar to the examples given to the train function (above) as possible. Because language
#' in different domains is used very differently the accuracy will generally drop as the
#' difference between this text and the training text increases. Base64 encoded image data, image urls, and
#' text content are all valid.
#' @param domain String: This is an identifier that helps determine the appropriate techniques for indico
#' to use behind the scenes to train your model.  One of {"standard", "topics"}
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param version for api version
#' @param ... additional arguments to passed to request
#' @return List with available collections
#' @export
#' @import httr rjson stringr
#' @examples
#' collection <- Collection(name='example')
#' test_data <- list(list("I love my friends!", "extrovert"),
#'                   list("I love to be alone", "introvert"),
#'                   list("I have mixed feelings on people", "ambivert"))
#' addData(collection, test_data)
#' train(collection)
#' wait(collection)
#' res = train("I love my friends!")
#' cat(sprintf("The likelihood the author was an extrovert is \%0.4f.",
#'             res[["extrovert"]])
setMethod(f="predict",
          signature="Collection",
          definition=function(collection_object, data, api_key = FALSE, cloud = FALSE, version = NULL, domain = NULL, ...) {
                data = format_image(data, 48)
                collection_object@domain <- ifelse(domain!=NULL, domain, collection_object@domain)
                make_request(data, 'custom', api_key, cloud, version, collection = collection_object@name, method='predict', domain=  collection_object@domain, ...)
          }
          )

setGeneric(name="remove_example",
           def=function(collection_object, data, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
              standardGeneric("remove_example")
           }
           )

#'  This is an API made to remove a single instance of training data. This is useful in cases where a
#'  single instance of content has been modified, but the remaining examples remain valid. For
#'  example, if a piece of content has been retagged.
#'
#' @param collection the collection object for this model
#' @param The exact text you wish to remove from the given collection. If the string
#' provided does not match a known piece of text then this will fail. Again, this is required if
#' an id is not provided, and vice-versa.
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param version for api version
#' @param ... additional arguments to passed to request
#' @return List with available collections
#' @export
#' @import httr rjson stringr
#' @examples
#' collection <- Collection(name='example')
#' test_data <- list(list("I love my friends!", "extrovert"),
#'                   list("I love to be alone", "introvert"),
#'                   list("I have mixed feelings on people", "ambivert"))
#' addData(collection, test_data)
#' remove_example(collection, test_data[[1]][[1]])
setMethod(f="remove_example",
          signature="Collection",
          definition=function(collection_object, data, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
                data = format_image(data, 48)
                make_request(data, 'custom', api_key, cloud, version, collection = collection_object@name, method='remove_example', ...)
          }
          )


#' This is a status report endpoint. It is used to get the status on all of the collections currently trained, as
#' well as some basic statistics on their accuracies. See docs for more information
#'
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param version for api version
#' @param ... additional arguments to passed to request
#' @return List with available collections
#' @export
#' @import httr rjson stringr
#' @examples
#' collections <- collections()
#'
#' cat(sprintf("There are currently %i collections",
#'             length(collections))
collections <- function(api_key = FALSE, cloud = FALSE, version = NULL, ...) {
  make_request(NULL, 'custom', api_key, cloud, version, method = "collections", ...)
}
