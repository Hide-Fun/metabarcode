context("label_extract")
library(metabr)

test_that("label_extract extract number from label", {
  expect_equal(label_extract("AB2-3-10SS"), "2-3-10")
  expect_equal(label_extract("AB2-40SS"), "2-40")
  expect_equal(label_extract("A3"), "3")
})

test_that("label_extract is not accept factor", {
  expect_error(label_extract(factor("AB2-3-10SS")), "string must be a character")
  expect_error(label_extract(factor("AB2-40SS")), "string must be a character")
  expect_error(label_extract(factor("A3")), "string must be a character")
})

test_that("label_extract is not accept vector", {
  expect_error(label_extract(c("AB7-8SS", "AB2-3-10SS")), "length(string) == 1 is not TRUE")
})
