context("personas")

categories <-c(
  "architect",
  "logician",
  "commander",
  "debater",
  "advocate",
  "mediator",
  "protagonist",
  "campaigner",
  "logistician",
  "defender",
  "executive",
  "consul",
  "virtuoso",
  "adventurer",
  "entreprenuer",
  "entertainer"
)
test_that("personas API returns dict with correct categories", {
  test_string <- "I love my friends!"
  personas <- personas(test_string)

  expect_is(personas, "list")
  expect_true(all(names(personas) %in% categories))
})

test_that("Throws error on empty text", {
  expect_error(personas())
  expect_error(personas(""))
  expect_error(personas(" "))
})

test_that("Batch personas API returns list of list of scores for the different personas axis", {
  test_string_arr <- c("I love my friends!", "I like to be alone.")
  personas <- personas(test_string_arr)

  expect_is(personas, "list")
  expect_is(personas[[1]], "list")
  expect_true(all(names(personas[[1]]) %in% categories))
})
