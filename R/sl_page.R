#' Specify a paper size for a page of labels
#'
#' This function takes a character string, such as 'A4' or 'letter' and returns the dimensions of the paper size in inches. To be used as a helper in functions like sl_labels()
#'
#' @return page_specification An UK paper size from "A1" to "A5", or any of "tabloid", "legal", "letter" or "half letter"
#' @value A numeric vector specifying width and height of the page in inches.
#'
#' @export

sl_page <- function(page_specification){

  stopifnot(length(page_specification) == 1 & is.character(page_specification))

  page_specification <- tolower(page_specification)

  if(!page_specification %in% c("a1", "a2", "a3", "a4", "a5", "tabloid", "legal", "letter", "half letter")){
    rlang::abort("page_specification must be one of c('A1', 'A2', 'A3', 'A4', 'A5', 'tabloid', 'legal', 'letter', 'half letter')")
  }

  uk_sizes <- c(841, 594, 420, 297, 210)

  if(page_specification %in% c("a1", "a2", "a3", "a4", "a5")){

    num_element <-
      gsub("a", "", page_specification) |>
      as.numeric()

    out <- c(uk_sizes[num_element + 1], uk_sizes[num_element])

    attr(out, "units") <- "mm"

  } else {
    out <-
      switch(page_specification,
             tabloid = c(11, 17),
             legal = c(8.5, 14),
             letter = c(8.5, 11),
             `half letter` = c(5.5, 8.5)
             )

    attr(out, "units") <- "in"
  }

  out
}

convert_sl_page_units <- function(page, units){

  if(units == "px") rlang::abort("Units are in pixels (px), so page size must be provided as a numeric vector denoting width and height in pixels.")

  p_units <- attr(page, "units")

  conversions <-
    list(
      mm = c(mm = 1, cm = 0.1, `in` =  0.0393701),
      cm = c(mm = 10, cm = 1, `in` = 0.393701),
      `in`= c(mm = 25.4, cm = 2.54, `in` = 1)
    )

  multiplier <- conversions[[p_units]][[units]]

  out <- page * multiplier

  attr(out, "units") <- units
  out
}


