context("Named Entities")

test_that("Named entites should return all entities with breakdown", {
  test_string <- "I want to move to New York"
  entities <- named_entities(test_string)

  expect_is(entities, "list")
  entity <- entities[[names(entities)[[1]]]]
  expected_categories <- c('unknown', 'organization', 'location', 'person')
  expect_equal(names(entity$categories), expected_categories)
  expect_equal(length(names(entity$categories)), 4)
  expect_equal(sum(unlist(entity$categories)), 1)
})

test_that("Named entities should return nothing with a threshold of 1", {
  test_string <- "I want to move to New York"
  entities <- named_entities(test_string, threshold = 1)
  expect_equal(length(entities), 0)
})

test_that("Throws error on empty text", {
  expect_error(named_entities())
  expect_error(named_entities(""))
  expect_error(named_entities(" "))
})

test_that("Batch named entites should return all entities with breakdown", {
  test_string <- c("I want to move to New York", "I would hate to move to Cleveland")
  docuements <- named_entities(test_string)

  expect_is(docuements, "list")
  entities <- docuements[[1]]
  expect_is(entities, "list")
  entity <- entities[[names(entities)[[1]]]]
  expected_categories <- c('unknown', 'organization', 'location', 'person')
  expect_equal(names(entity[['categories']]), expected_categories)
  expect_equal(length(names(entity[['categories']])), 4)
  expect_equal(sum(unlist(entity[['categories']])), 1)
})

test_that("Batch named entities should return nothing with a threshold of 1", {
  test_string <- c("I want to move to New York", "I would hate to move to Cleveland")
  docuements <- named_entities(test_string, threshold = 1)
  entities <- docuements[[1]]
  expect_equal(length(entities), 0)
})
