test_that("Create ngchm with row/col label display length set", {
  chm <- chmNew("test", rowDisplayLength = 7, colDisplayLength = 12)
  expect_equal(chm@rowDisplayLength, 7)
  expect_equal(chm@colDisplayLength, 12)
})

test_that("Change row/col label display length in ngchm", {
  chm <- chmNew("test")
  expect_equal(chm@rowDisplayLength, 20) #<-- default is 20
  expect_equal(chm@colDisplayLength, 20) #<-- default is 20
  chm <- chmSetDisplayLength(chm, 17, "row")
  chm <- chmSetDisplayLength(chm, 24, "col")
  expect_equal(chm@rowDisplayLength, 17)
  expect_equal(chm@colDisplayLength, 24)
})

test_that("Error thrown if non-integer value is passed or row/col not correct", {
  chm <- chmNew("test")
  expect_error(chmSetDisplayLength(chm, "a", "row"))
  expect_error(chmSetDisplayLength(chm, 1.5, "col"))
  expect_error(chmSetDisplayLength(chm, 10, "not row or col"))
  expect_error(chmSetDisplayLength(chm, 10))
  expect_error(chmSetDisplayLength(chm))
})
