test_that("Testing function convertPanelSelectionsFromLabelToIndex", {
  ## Testing that the selections made via row and column labels
  ## are converted to indices in the chm object.
  ##
  ## There are three entities to test:
  ##   1. hclust
  ##   2. dendrogram
  ##   3. label
  matrix_data <- matrix(c(5, 2, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    nrow = 4, ncol = 3,
    dimnames = list(c("row1", "row2", "row3", "row4"), c("col1", "col2", "col3"))
  )

  # 1. `hclust` test: cluster rows and create `hclust` object to send to chmNew
  distRows <- stats::dist(matrix_data, method = "euclidean")
  hclustRows <- stats::hclust(distRows, method = "complete")

  # 2. `dendrogram` test: cluster columns and `dendrogram` object to send to chmNew
  distCols <- stats::dist(t(matrix_data), method = "euclidean")
  ddgColumns <- stats::as.dendrogram(stats::hclust(distCols, method = "complete"))

  # Create panels in order to specify selections
  panes_list <- list(pane(id = "pane1"), pane(id = "pane2"), sizes = 50, 50)
  pane_types <- list(
    pane1 = detailMap(id = "pane1"),
    pane2 = summaryMap(id = "pane2")
  )
  selections <- list(row = list("row2", "row3"), col = list("col1", "col2"))
  panel_config <- panel_configuration(panes_list, pane_types, selections = selections)

  # Initialize the chm object
  chm <- chmNew("test", matrix_data,
                panel_configuration = panel_config,
                rowOrder = hclustRows,
                colOrder = ddgColumns
         )

  # Calling `chmMake` to call `convertPanelSelectionsFromLabelToIndex()`, which converts
  # the selections made via row and column labels to indices in the chm object.
  chm <- chmMake(chm)

  # This clustering is deterministic, so we can check the indices:
  expect_equal(list(4, 1), chm@panel_configuration@selections$row)
  expect_equal(list(2, 3), chm@panel_configuration@selections$col)

  # 3. `label` test: use don't do any clustering
  chm <- chmNew("test", matrix_data,
                panel_configuration = panel_config,
                rowOrder = rownames(matrix_data),
                colOrder = colnames(matrix_data)
         )
  chm <- chmMake(chm)
  expect_equal(list(2, 3), chm@panel_configuration@selections$row)
  expect_equal(list(1, 2), chm@panel_configuration@selections$col)
})
