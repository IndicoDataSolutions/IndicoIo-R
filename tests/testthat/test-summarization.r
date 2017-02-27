context("Summarization API")

test_that("Summarization API returns sentences contained in original text.", {
  test_string <- "First sentence. Second sentence. Third sentence."
  summary <- summarization(test_string, top_n=2)
  expect_is(summary, "list")
  expect_equal(length(summary), 2)
  expect_true(grepl(summary[[1]], test_string))
  expect_true(grepl(summary[[2]], test_string))
})
