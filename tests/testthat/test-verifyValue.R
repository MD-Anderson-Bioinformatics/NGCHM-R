test_that("Testing verifyValue function", {
  test_value <- 22
  valid_values <- c(10.1, 15.3, 20.9, 25.7)
  warning_message <- "Test message"
  verified_value <- expect_warning(verifyValue(
                                     test_value,
                                     validValues = valid_values,
                                     warning_message = warning_message),
                     regexp = warning_message)
  expect_equal(verified_value, 20.9) #<-- 22 is rounded to 20.9

  verified_value <- expect_error(verifyValue(test_value), regexp = "Missing argument 'validValues'")
})
