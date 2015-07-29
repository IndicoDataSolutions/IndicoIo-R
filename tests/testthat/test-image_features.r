context("Image Features Detection")

test_that("Image Features Detection API returns proper answer", {

  test_image <- paste(readLines("image/base64.txt"), collapse=" ")
  features <- image_features(test_image)

  expect_is(features, "list")
  expect_equal(length(features), 2048)
})

test_that("Throws error on empty or wrong image", {
  expect_error(image_features())
  expect_error(image_features(array(10)))
})

test_that("Batch Image Features Detection API returns proper answer", {

  test_image_arr <- list()
  test_image_arr[[1]] = paste(readLines("image/base64.txt"), collapse=" ")

  features <- image_features(test_image_arr)

  expect_is(features, "list")
  expect_is(features[[1]], "list")
  expect_equal(length(features), length(test_image_arr))
  expect_equal(length(features[[1]]), 2048)
})
