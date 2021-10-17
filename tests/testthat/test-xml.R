test <- .xmlAddChunks(outlist = list(), element = "foo", attributeList = list("bar" = 1, "baz" = 1))
expected <- list(foo = list("bar" = 1, "baz" = 1))

test_that(".xmlAddChunks is working correctly", {
  expect_equal(test, expected)
})

test_that(".xmlAddChunks throws wrong length error", {
  expect_error(.xmlAddChunks(outlist = list(), element = "foo", attributeList = list("bar" = c(1, 2), "baz" = 1)),
               "Value vectors in attributeList need to be of same length")
})
