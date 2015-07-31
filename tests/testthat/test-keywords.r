context("keywords")

test_that("Keywords API returns list of scores for the list of keywords", {
  test_string <- "Hoping for sunshine tomorrow."
  keywords <- keywords(test_string)

  expect_is(keywords, "list")
  expect_equal(length(keywords), 3)
})

test_that("Throws error on empty text", {
  expect_error(language())
  expect_error(language(""))
  expect_error(language(" "))
})

test_that("Batch Text Tags API returns list of list of probabilities for the list of topics", {
  test_string_arr <- c("Hoping for sunshine tomorrow.", "Hoping for rain tomorrow.")
  keywords <- keywords(test_string_arr)

  expect_is(keywords, "list")
  expect_is(keywords[[1]], "list")
  expect_equal(length(keywords), 2)
  expect_equal(length(keywords[[1]]), 3)
})
