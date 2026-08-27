

test_fun <- function(paper_size = c(50, 50), label_size = c(5, 5), ...){
  sl_labels(paper_size, label_size, ...)
}

test_that("sl_labels() doesn't crash with reasonable inputs", {
  expect_no_error(test_fun)
})

test_that("sl_labels() is sufficiently strict about inputs", {

  expect_error(test_fun(paper_size = "foo"))
  expect_error(test_fun(paper_size = NULL))

  expect_error(test_fun(paper_size = 1:3), class = "invalid_dimensions")
  expect_error(test_fun(label_size = c(-1, 2)), class = "invalid_dimensions")
  expect_error(test_fun(margin = "foo"), class = "invalid_dimensions")
  expect_error(test_fun(padding = NULL), class = "invalid_dimensions")

})

test_that("Labels and paper are not allowed to have 0 width/height", {

  expect_error(test_fun(paper_size = c(0, 0)), class = "zero_dimension")
  expect_error(test_fun(label_size = c(0, 0)), class = "zero_dimension")

})

test_that("Physical page dimensions can not be specified for labels whose dimensions are specified in pixels", {

  expect_error(test_fun(paper_size = "a4", units = "px"))

})

test_that("Units are successfully converted when specifying pages via sl_page()", {

  result_mm <- test_fun(paper_size = "A4", label_size = c(63, 46.5), units = "mm")
  result_cm <- test_fun(paper_size = "A4", label_size = c(6.3, 4.65), units = "cm")
  result_in <- test_fun(paper_size = "A4", label_size = c(2.48, 1.83), units = "in")

  expect_equal(result_mm$x_min/10, result_cm$x_min, tolerance = 0.1)
  expect_equal(result_cm$x_min, result_in$x_min * 2.54, tolerance = 0.1)

  })
