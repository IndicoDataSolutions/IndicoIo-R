context("Collection")

test_string <- "unit_test_collection"
alternate_name <- "alterate_unit_test_collection"
test_data <- list(list("I love my friends!", "extrovert"),
                  list("I love to be alone", "introvert"),
                  list("I have mixed feelings on people", "ambivert"))
test_image_data <- list(list('image/image.png', .5), list('image/not_square.png', 1.5))

clearTests <- function(collection) {
  try({
    deregister(collection)
  }, silent=TRUE)
  try({
    clear(collection)
  }, silent=TRUE)
}

test_that("instantiate a collection and add data, and predict with text", {
  collection <- Collection(name=test_string)
  clearTests(collection)

  addData(collection, test_data)
  addData(collection, list("I love my friends!", "extrovert"))
  train(collection)
  wait(collection)
  prediction <- predict(collection, "I love my friends!")
  expect_equal(prediction[["extrovert"]] > .5, TRUE)
  prediction <- predict(collection, list("I love my friends!", "I love to be alone"))
  expect_true(prediction[[1]][["extrovert"]] > prediction[[2]][["extrovert"]])
  clearTests(collection)
})

test_that("instantiate a collection and add data, and predict with images", {
  collection <- Collection(name=test_string)
  clearTests(collection)

  addData(collection, test_image_data)
  train(collection)
  wait(collection)
  prediction <- predict(collection, "image/image.png")

  expect_is(prediction, "numeric")
  clearTests(collection)
})

test_that("instantiate a collection and add large batch", {
  collection <- Collection(name=test_string)
  clearTests(collection)

  addData(collection, rep(test_data, 100))
  train(collection)
  wait(collection)
  prediction <- predict(collection, "I love my friends!")
  expect_true(prediction[["extrovert"]] > .5)
  clearTests(collection)
})

test_that("instantiate a collection and remove an example", {
  collection <- Collection(name=test_string)
  clearTests(collection)

  addData(collection, test_data)
  train(collection)
  wait(collection)
  prediction <- predict(collection, "I love my friends!")
  expect_true('extrovert' %in% names(prediction))

  remove_example(collection, test_data[[1]][[1]])
  train(collection)
  wait(collection)
  prediction <- predict(collection, "I love my friends!")
  expect_false('extrovert' %in% names(prediction))

  clearTests(collection)
})

test_that("instantiate a collection and clear it", {
  collection <- Collection(name=test_string)
  clearTests(collection)

  addData(collection, test_data)
  train(collection)
  wait(collection)
  collections <- collections()
  expect_false(is.null(collections[[test_string]]))
  clear(collection)
  collections <- collections()
  expect_true(is.null(collections[[test_string]]))
  clearTests(collection)
})

test_that("collection can be renamed", {
  collection <- Collection(name=test_string)
  alternateCollection <- Collection(name=alternate_name)
  clearTests(collection)
  clearTests(alternateCollection)

  addData(collection, test_data)
  train(collection)
  wait(collection)
  collection <- rename(collection, alternate_name)
  info(collection)
  clear(collection)
})

test_that("collection registration", {
  collection <- Collection(name=test_string)
  clearTests(collection)

  addData(collection, test_data)
  train(collection)
  wait(collection)
  register(collection)
  status <- info(collection)
  expect_true(status[['registered']])
  expect_false(status[['public']])
  deregister(collection)
  status <- info(collection)
  expect_false(status[['registered']])
  expect_false(status[['public']])
  clear(collection)
})

test_that("collection registration (public)", {
  collection <- Collection(name=test_string)
  clearTests(collection)

  addData(collection, test_data)
  train(collection)
  wait(collection)
  register(collection, make_public=TRUE)
  status <- info(collection)
  expect_true(status[['registered']])
  expect_true(status[['public']])
  deregister(collection)
  status <- info(collection)
  expect_false(status[['registered']])
  expect_false(status[['public']])
  clear(collection)
})

test_that("collection authorization", {
  collection <- Collection(name=test_string)
  clearTests(collection)

  addData(collection, test_data)
  train(collection)
  wait(collection)
  register(collection)
  status <- info(collection)
  expect_true(status[['registered']])
  expect_false(status[['public']])
  authorize(collection, 'contact@indico.io')
  status <- info(collection)
  expect_true('contact@indico.io' %in% status[['permissions']][['read']])
  deauthorize(collection, 'contact@indico.io')
  deregister(collection)
  status <- info(collection)
  expect_false('contact@indico.io' %in% status[['permissions']][['read']])
  expect_false(status[['registered']])
  expect_false(status[['public']])
  clear(collection)
})
