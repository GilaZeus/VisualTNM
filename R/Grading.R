#' combine 1/2 with 2, 2/3 with 3 and 3/4 with 4 for grading.
#'
#' @param data A tibble
combine_diff <- function(data, D) {
  D <- rlang::enquo(D)
  data %>%
    dplyr::mutate(!!D := as.character(!!D)) %>%
    dplyr::mutate(!!D := ifelse(!!D == "1/2", "2", !!D)) %>%
    dplyr::mutate(!!D := ifelse(!!D == "2/3", "3", !!D)) %>%
    dplyr::mutate(!!D := ifelse(!!D == "3/4", "4", !!D)) %>%
    dplyr::mutate(!!D := as.factor(!!D))
  }


#' Create dataset for grading's plots (only for 4 separate grading levels: 1, 2, 3 and 4)
#'
#' @param data A tibble with Grading and their count
#' @param D Column with grading
#' @param thickness Tissue's width given in cell count
#' @param width Cell width
#' @param height Cell height
#' @param base_noise Base noise for all grading steps
#' @param noise_multipliers Multipliers for every grading level
#' @return A tibble with coordinates of each cell within the tissue, their grading status and cell nucleous size
create_diff_dataset <- function(data, D, thickness = 10, width = 4, height = 2.5, base_noise = 0.55, noise_multipliers = c(0, 1, 2, 4)) {
  D <- rlang::enquo(D)
  if(dplyr::n_distinct(data[, rlang::as_name(D)]) != length(noise_multipliers) | dplyr::n_distinct(data[, rlang::as_name(D)]) != 4) {
    stop("Wrong number of grading steps!")
  }

  data %>%
    create_coord_dataset(thickness, width, height) %>%
    tibble::add_column(rand_x = runif(nrow(.), min = -base_noise, max = base_noise)) %>%
    tibble::add_column(rand_y = runif(nrow(.), min = -base_noise, max = base_noise)) %>%
    tibble::add_column(nucl_r = runif(nrow(.), min = base_noise / 2, max = base_noise)) %>%
    dplyr::mutate(nucl_r = ifelse(!!D == "1", runif(nrow(.), min = base_noise / (2 + noise_multipliers[1]), max = base_noise * (2 + noise_multipliers[1])), nucl_r)) %>%
    dplyr::mutate(nucl_r = ifelse(!!D == "2", runif(nrow(.), min = base_noise / (2 + noise_multipliers[2]), max = base_noise * (2 + noise_multipliers[2])), nucl_r)) %>%
    dplyr::mutate(nucl_r = ifelse(!!D == "3", runif(nrow(.), min = base_noise / (2 + noise_multipliers[3]), max = base_noise * (2 + noise_multipliers[3])), nucl_r)) %>%
    dplyr::mutate(nucl_r = ifelse(!!D == "4", runif(nrow(.), min = base_noise / (2 + noise_multipliers[4]), max = base_noise * (2 + noise_multipliers[4])), nucl_r)) %>%
    dplyr::mutate(x = ifelse(!!D == "1", x + rand_x * noise_multipliers[1], x)) %>%
    dplyr::mutate(x = ifelse(!!D == "2", x + rand_x * noise_multipliers[2], x)) %>%
    dplyr::mutate(x = ifelse(!!D == "3", x + rand_x * noise_multipliers[3], x)) %>%
    dplyr::mutate(x = ifelse(!!D == "4", x + rand_x * noise_multipliers[4], x)) %>%
    dplyr::mutate(y = ifelse(!!D == "1", y + rand_y * noise_multipliers[1], y)) %>%
    dplyr::mutate(y = ifelse(!!D == "2", y + rand_y * noise_multipliers[2], y)) %>%
    dplyr::mutate(y = ifelse(!!D == "3", y + rand_y * noise_multipliers[3], y)) %>%
    dplyr::mutate(y = ifelse(!!D == "4", y + rand_y * noise_multipliers[4], y)) %>%
    dplyr::select(-rand_x, -rand_y)
}
