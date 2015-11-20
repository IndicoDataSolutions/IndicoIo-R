context("myers-briggs")

categories <-c(
  "I",
  "E",
  "N",
  "S",
  "T",
  "F",
  "J",
  "P",
  "personality"
)
test_that("Myers Briggs API returns dict with correct categories", {
  test_string <- "I love my friends!"
  myers_briggs <- myers_briggs(test_string)

  expect_is(myers_briggs, "list")
  expect_true(all(names(myers_briggs) %in% categories))
})

test_that("Throws error on empty text", {
  expect_error(myers_briggs())
  expect_error(myers_briggs(""))
  expect_error(myers_briggs(" "))
})

test_that("Batch myers_briggs API returns list of list of probabilities for the list of topics", {
  test_string_arr <- c("I love my friends!", "I like to be alone.")
  myers_briggs <- myers_briggs(test_string_arr)

  expect_is(myers_briggs, "list")
  expect_is(myers_briggs[[1]], "list")
  expect_true(all(names(myers_briggs[[1]]) %in% categories))
  expect_equal(sum(unlist(myers_briggs[[1]])), 1)
})
