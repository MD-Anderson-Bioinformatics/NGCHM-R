test_that("testing validateColor()", {
  expect_equal("yellow", validateColor("yellow"))
  expect_error(validateColor("not-a-color"), "Color 'not-a-color' must be a valid color name or hexadecimal color.")
  expect_warning(validateColor("#00000000"), "Alpha channel is not supported and will be removed from '#00000000'.")
})
