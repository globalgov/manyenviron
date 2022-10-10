# Test generate_agreements()

test_that("lenght of random list is correct", {
  expect_length(generate_agreements(12), 12)
  expect_equal(class(generate_agreements(1)), "character")
  set.seed(1234)
  expect_length(unique(generate_agreements(12)), 12)
})
