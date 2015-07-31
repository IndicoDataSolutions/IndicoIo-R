context("Text tags")

test_that("Text Tags API returns list of probabilities for the list of topics", {
  test_string <- "Hoping for sunshine tomorrow."
  tags <- text_tags(test_string)

  expect_is(tags, "list")
  expect_equal(length(tags), 111)
  expect_equal(sum(unlist(tags)), 1)
})

test_that("Throws error on empty text", {
  expect_error(language())
  expect_error(language(""))
  expect_error(language(" "))
})

test_that("Batch Text Tags API returns list of list of probabilities for the list of topics", {
  test_string_arr <- c("Hoping for sunshine tomorrow.", "Hoping for rain tomorrow.")
  tags <- text_tags(test_string_arr)

  expect_is(tags, "list")
  expect_is(tags[[1]], "list")
  expect_equal(length(tags), 2)
  expect_equal(length(tags[[1]]), 111)
  expect_equal(sum(unlist(tags[[1]])), 1)
})

test_that("Batch Text Tags API keyword arguments function", {
  test_string_arr <- c("Hoping for sunshine tomorrow.", "Hoping for rain tomorrow.")
  tags <- text_tags(test_string_arr, top_n = 5)

  expect_is(tags, "list")
  expect_is(tags[[1]], "list")
  expect_equal(length(tags), 2)
  expect_equal(length(tags[[1]]), 5)
})
