context("Keywords v2")

test_that("Keywords API returns list of scores for the list of keywords", {
  test_string <- "hoping for sunshine tomorrow"
  keywords <- keywords(test_string, version = 2)

  expect_is(keywords, "list")
  for (word in names(keywords)) {
    expect_true(grepl(word, test_string))
  }

})

test_that("Batch Keywords API returns list of list of probabilities for the list of topics", {
  test_string_arr <- c("hoping for sunshine tomorrow", "hoping for rain tomorrow")
  keywords <- keywords(test_string_arr, version = 2)

  expect_is(keywords, "list")
  expect_is(keywords[[1]], "list")
  for (word in names(keywords[[1]])) {
    expect_true(grepl(word, test_string_arr[[1]]))
  }
})
