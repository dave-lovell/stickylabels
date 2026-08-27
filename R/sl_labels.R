
area <- \(x){
  stopifnot(length(x) == 2)
  stopifnot(is.numeric(x))

  x[1] * x[2]
}


# TODO: convert paper size to appropriate units


#label_dimensions <-

  function(
    paper_size,
    label_size,
    margin,
    padding = c(TRUE, TRUE),
    units = "cm"){

    excluding_margin <- paper_size - 2 * margin

    padding <- (excluding_margin %% label_size) * padding

    n_labels <- round((excluding_margin - padding) / label_size)

    mins <- vector("list", 2)
    names(mins) <- c("x", "y")

    for(i in seq_along(mins)){

      distance_between <- label_size[[i]] + padding[[i]]/(n_labels[[i]] - 1)
      multiplier <- 1:n_labels[[i]] - 1

      mins[[i]] <- margin[[i]] + distance_between * multiplier
    }

    label_dims <-
      do.call(expand.grid, mins) |>
      tibble::as_tibble() |>
      rename(x_min = 1, y_min = 2)

    label_dims$x_max <- label_dims$x_min + label_size[[1]]
    label_dims$y_max <- label_dims$y_min + label_size[[2]]

    label_dims$x_centroid <- (label_dims$x_min + label_dims$x_max) / 2
    label_dims$y_centroid <- (label_dims$y_min + label_dims$y_max) / 2


    label_dims$label_number <- 1:area(n_labels)

    n_cols <- ncol(label_dims)

    label_dims <- cbind(label_dims[n_cols], label_dims[c(1:(n_cols-1))])

    class(label_dims) <- c(class(label_dims), "sl_label_page")
    attr(label_dims, "units") <- units
  }



check_valid_dimensions <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()){

  valid_dimensions <- length(x) == 2 && is.numeric(x) && all(x >= 0)

  if(!valid_dimensions){
    cli::cli_abort(
      "{.arg {arg}} must be a length-two, non-negative numeric vector.",
      call = call
      )
  }
}

#' Create a tibble which defines the dimensions of sticky labels on a page
#'
#' Returns a tibble in which each observation corresponds to one sticky label, and variables define dimensions
#'
#' @return a 'stickylabels' tibble
#'
#' @param paper_size A call to sl_paper() (e.g. `sl_paper("A4")`) or a length-two numeric vector specifying paper height and width in `units`.
#' @param label_size A length-two numeric vector specifying label height and width in `units`.
#' @param margin A length-two numeric vector specifying the horizontal and vertical page margin (unused space at the edge of the page).
#' @param padding A length-two numeric vector specifying the horizontal and vertical padding between each label.
#' @param units A character string passed to [ggplot2::ggsave()]. One of `c("mm", "cm" "in", "px")`.
#'
#' @export
#'
#

sl_label_page <- function(paper_size, label_size, margin, padding, units = "mm"){

  check_valid_dimensions(paper_size)
  check_valid_dimensions(label_size)
  check_valid_dimensions(margin)
  check_valid_dimensions(padding)

  stopifnot(length(units) == 1 && is.character(units))
  stopifnot(units %in% c("in", "cm", "mm", "px"))

  if(any(label_size + padding + margin > paper_size)) rlang::abort("It's not possible to fit those labels on this paper. Specify a larger paper_size, or a smaller label_size, margin or padding argument.")

  label_dimensions(paper_size, label_size, margin, padding, units)
}
