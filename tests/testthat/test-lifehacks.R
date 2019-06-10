#---- prase_lifehacks---####

test_that("parsing Life Hacks works", {

  x <- "0, Acknowledge that your abilities are fluid, Challenge your motivated reasoning, Analyze your own beliefs, Respect and understand other people's elephants"
  var <- "helo"

  results <- parse_lifehacks(x, var)

  expect_equal(results$helo1, "0")
  expect_equal(results$helo2, "Acknowledge that your abilities are fluid")
  expect_equal(results$helo3, "Challenge your motivated reasoning")
  expect_equal(results$helo4, "Analyze your own beliefs")
  expect_equal(results$helo5, "Respect and understand other people's elephants")
})
