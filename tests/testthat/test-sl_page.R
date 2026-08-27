test_that("sl_page() crashes with bad input", {
  expect_error(sl_page("a500"))
  expect_error(sl_page())
  expect_error(sl_page(4))
  expect_error(sl_page(c("A3", "A4")))
  expect_no_error(sl_page("A4"))
})

test_that("sl_page() returns 2-length numeric with units", {
  out <- sl_page("A4")
  out_attr <- attributes(out)

  expect_identical(names(out_attr), "units")
  expect_length(out, 2)
})

