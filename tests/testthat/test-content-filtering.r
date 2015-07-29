context("Content Filtering")

test_that("Content Filtering API returns proper answer", {

  test_image <- paste(readLines("image/base64.txt"), collapse=" ")
  probability <- content_filtering(test_image)

  expect_is(probability, "numeric")
  expect_more_than(.5, probability)
})

test_that("Throws error on empty or wrong image", {
  expect_error(content_filtering())
  expect_error(content_filtering(array(10)))
})

test_that("Batch Content Filtering API returns proper answer", {

  test_image_arr <- list()
  test_image_arr[[1]] = paste(readLines("image/base64.txt"), collapse=" ")

  probabilities <- content_filtering(test_image_arr)

  expect_is(probabilities, "list")
  expect_is(probabilities[[1]], "numeric")
  expect_equal(length(probabilities), length(test_image_arr))
  expect_more_than(.5, probabilities[[1]])
})
