library(NGCHM)
context("git digest")

test_that("gitSha matches git sha computation", {
  # Test cases from http://stackoverflow.com/questions/552659/assigning-git-sha1s-without-git
  expect_equal(gitSha(""), "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391")
  expect_equal(gitSha("foobar\n"), "323fae03f4606ea9991df8befbb2fca795e648fa")
})
