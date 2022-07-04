#' Generate dim(data)[1] points around center with c0 coordinates and r radius.
#'
#' @param data A dataframe
#' @param c0 Center point of the generated circle
#' @param r Circle radius
#' @return Same dataset with new x and y columns
#' @export
create_circle_coords <- function(data, c0 = c(0, 0), r = 1) {
  if (length(c0) != 2) {
    stop("c0 must me a vector of length 2.")
  }
  n <- dplyr::summarise(data, x = sum(n))[1, "x", 1]
  if (n < 3) {
    stop("At least 3 points (n = 3) are needed to be generated!")
  }
  if (r <= 0) {
    stop("Incorrect radius.")
  }
  data <- tidyr::uncount(data, n)

  data[, "x"] <- NA
  data[, "y"] <- NA

  a = 2 * pi / n
  for (i in 1 : n) {
    data[i, "x"] <- c0[1] + r * cos(pi/ 2 - i * a)
    data[i, "y"] <- c0[2] + r * sin(pi/ 2 - i * a)
    data[i + n, "x"] <- c0[1] + r * cos(pi/ 2 - i * a)
    data[i + n, "y"] <- c0[2] + r * sin(pi/ 2 - i * a)
  }

  return(data)
}

#' Draw a donut graph with round edges
#'
#' @param data A dataframe
#' @param col_group A column that divides data in **two** groups
#' @param col_feature A column containing information to be plotted
#' @return A dataset appropriate for donut plotting
#' @export
create_donut <- function(data, col_group, col_feature) {
  col_group <- rlang::enquo(col_group)
  col_feature <- rlang::enquo(col_feature)

  data <-
    data %>%
    dplyr::group_by(!!col_group) %>%
    calc_freq(!!col_feature) %>%
    new_size(total_num = 2500) %>%
    create_circle_coords()

  data %>%
    dplyr::bind_rows(data[c(1, 2501),])
}
