test_that("Create ngchm with row/col label display length set", {
  chm <- expect_warning(chmNew("test", rowDisplayLength = 7, colDisplayLength = 12))
  expect_equal(chm@rowDisplayLength, 10) #<-- converted to nearest of .pkg_env$allowed_display_lengths
  expect_equal(chm@colDisplayLength, 10) #<-- converted to nearest of .pgk_env$allowed_display_lengths
})

test_that("Change row/col label display length in ngchm", {
  chm <- chmNew("test")
  expect_equal(chm@rowDisplayLength, 20) #<-- default is 20
  expect_equal(chm@colDisplayLength, 20) #<-- default is 20
  chm <- expect_warning(chmSetDisplayLength(chm, 17, "row"))
  chm <- expect_warning(chmSetDisplayLength(chm, 24, "col"))
  expect_equal(chm@rowDisplayLength, 15) #<-- converted to nearest of .pkg_env$allowed_display_lengths
  expect_equal(chm@colDisplayLength, 25) #<-- converted to nearest of .pkg_env$allowed_display_lengths
})
