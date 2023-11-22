library(NGCHM)

test_that("Creating simple ngchm does not error", {
  data_matrix <- matrix(1:12,
    nrow = 4, ncol = 3,
    dimnames = list(c("row1", "row2", "row3", "row4"), c("col1", "col2", "col3"))
  )
  chm <- chmNew("test ngchm", data_matrix)
  expect_is(chm, "ngchmVersion2")
})
