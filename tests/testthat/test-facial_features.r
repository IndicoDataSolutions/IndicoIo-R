context("Facial Features Detection")

test_that("Facial Features Detection API returns proper answer", {
  test_image <- matrix(runif(48*48, 0, 1), nrow = 48)
  features <- facial_features(test_image)
  
  expect_is(features, "list")
  expect_equal(length(features), 48)
})

test_that("facial_features alias also function", {
  test_image <- matrix(runif(48*48, 0, 1), nrow = 48)
  features <- facial_features(test_image)
  
  expect_is(features, "list")
  expect_equal(length(features), 48)
})

test_that("Throws error on empty or wrong image", {
  expect_error(facial_features())
  expect_error(facial_features(array(10)))
})

test_that("Batch Facial Features Detection API returns proper answer", {
  test_image_list = list()
  test_image_list[[1]] <- matrix(runif(48*48, 0, 1), nrow = 48)
  features <- batch_facial_features(test_image_list)
  
  expect_is(features, "list")
  expect_is(features[[1]], "list")
  expect_equal(length(features), length(test_image_list))
  expect_equal(length(features[[1]]), 48)
})

