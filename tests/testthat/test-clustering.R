test_that("Inf's in distance matrix", {
  ## euclidean distance for matrix_data will have Inf's in columns 1, 2, and 3
  matrix_data <- matrix(c(Inf, 2, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    nrow = 4, ncol = 3,
    dimnames = list(c("row1", "row2", "row3", "row4"), c("col1", "col2", "col3"))
  )
  chm <- chmNew("test", matrix_data, rowDist = "euclidean", colDist = "euclidean")
  expecedError <- "Unable to cluster columns. Distance matrix has 3 rows/cols with NA/NaN/Inf values. \n  Distance matrix rows/cols with NA/NaN/Inf values: col1, col2, col3"
  expect_error(chmDefaultColOrder(chm), expecedError)
})

test_that("NA's in distance matrix", {
  ## euclidean distance for matrix_data will have NA's in rows 1 and 3
  matrix_data <- matrix(c(NA, 2, 4, 4, NA, 6, 7, 8, 9, 10, NA, 12),
    nrow = 4, ncol = 3,
    dimnames = list(c("row1", "row2", "row3", "row4"), c("col1", "col2", "col3"))
  )
  chm <- chmNew("test", matrix_data, rowDist = "euclidean", colDist = "euclidean")
  expecedError <- "Unable to cluster rows. Distance matrix has 2 rows/cols with NA/NaN/Inf values. \n  Distance matrix rows/cols with NA/NaN/Inf values: row1, row3"
  expect_error(chmDefaultRowOrder(chm), expecedError)
})
