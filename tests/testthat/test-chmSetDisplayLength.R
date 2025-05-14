test_that("Create ngchm with row/col label display length", {
  nrow <- 30; ncol <- 30
  matrix_data <- matrix(rnorm(nrow * ncol), nrow = nrow, ncol = ncol)
  rownames(matrix_data) <- paste("Row", 1:nrow)
  colnames(matrix_data) <- paste("Col", 1:ncol)
  # expect warning about changing 7 -> 10 and 12 -> 10
  chm <- expect_warning(chmNew("test", matrix_data, rowDisplayLength = 7, colDisplayLength = 12))
  expect_equal(chm@rowDisplayLength, 10) #<-- converted to nearest of 10, 15, 20, 25, 30, 35, 40
  expect_equal(chm@colDisplayLength, 10) #<-- converted to nearest of 10, 15, 20, 25, 30, 35, 40
})

test_that("Add row/col label display length to ngchm", {
  nrow <- 30; ncol <- 30
  matrix_data <- matrix(rnorm(nrow * ncol), nrow = nrow, ncol = ncol)
  rownames(matrix_data) <- paste("Row", 1:nrow)
  colnames(matrix_data) <- paste("Col", 1:ncol)
  chm <- chmNew("test", matrix_data)
  expect_equal(chm@rowDisplayLength, 20) #<-- default is 20
  expect_equal(chm@colDisplayLength, 20) #<-- default is 20
  chm <- expect_warning(chmSetDisplayLength(chm, 17, "row"))
  chm <- expect_warning(chmSetDisplayLength(chm, 24, "col"))
  expect_equal(chm@rowDisplayLength, 15) #<-- converted to nearest of 10, 15, 20, 25, 30, 35, 40
  expect_equal(chm@colDisplayLength, 25) #<-- converted to nearest of 10, 15, 20, 25, 30, 35, 40
})
