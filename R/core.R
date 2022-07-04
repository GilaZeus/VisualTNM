#' Calculate UICC stage
#'
#' @param data A tibble
#' @param T Tumor size column name, values: "1", "2", "3", "4"
#' @param N Nodal status column name, values: "0", "1", "2", "3"
#' @param M Metastases column name, values: "M0", "M1"
#' @return Same data with new UICC column containing UICC stage. If UICC cannot be calculated, it will be filled with NA
calculate_UICC <- function(data, T, N, M) {
  T <- rlang::enquo(T)
  N <- rlang::enquo(N)
  M <- rlang::enquo(M)

  data %>%
    dplyr::mutate(UICC = ifelse(is.na(!!M),
                                NA,
                                ifelse(!!M == "M1",
                                       "IV",
                                       ifelse(is.na(!!N),
                                              NA,
                                              ifelse(!!N != "0",
                                                     "III",
                                                     ifelse(is.na(!!T),
                                                            NA,
                                                            ifelse(!!T == "3" | !!T == "4",
                                                                   "II",
                                                                   "I")))))))
}


#' Create age groups
#'
#' @param data A tibble with age column
#' @param age_name Name of the column with age data
#' @param res_name Name of the created column with age groups
#' @param ... Age group borders in increasing order
#' @return Same data with new res_name column containing age group info
create_age_groups <- function(data, age_name, res_name, ...) {
  age_name <- rlang::enquo(age_name)
  res_name <- rlang::enquo(res_name)

  data <-
    data %>%
    dplyr::mutate(!!res_name := NA)


  i <- 0
  vec <- c(...)
  for (group in vec) {
    if (i < group) {
      i <- group
    } else {
      stop("Incorrect age borders!")
    }

    data <-
      data %>%
      dplyr::mutate(!!res_name := ifelse(is.na(!!res_name) & !!age_name < group,
                                         paste("<", group, sep = " "),
                                         !!res_name))
  }

  data %>%
    dplyr::mutate(!!res_name := ifelse(is.na(!!res_name),
                                       paste("≥", i, sep = " "),
                                       !!res_name))
}


#' Calculate frequencies of `col`-values.
#'
#' @param data A tibble
#' @param col Column name for which frequencies are calculated
#' @param omit_NA If TRUE, ignores NAs.
#' @return Tibble with group names, their quantity (n) and frequencies (freq)
calc_freq <- function(data, col, omit_NA = TRUE) {
  col <- rlang::enquo(col)
  if (omit_NA) {
    data %>%
      dplyr::filter(!is.na(!!col)) %>%
      dplyr::count(!!col) %>%
      dplyr::mutate(freq = n / sum(n))
  } else {
    data %>%
      dplyr::count(!!col) %>%
      dplyr::mutate(freq = n / sum(n))
  }
}


#' Change dataset size based on frequencies
#'
#' @param data A tibble with a "freq"-column
#' @param total_num New size of the dataset
#' @param highlight_small If TRUE, adds +1 by rounding to the smallest frequencies at first; starts with the biggest by FALSE.
#' @return same dataset with a new n column with fixed numbers
new_size <- function(data, total_num = 100, highlight_small = TRUE) {
  data <-
    data %>%
    dplyr::mutate(n = as.integer(floor(freq * total_num)))

  data <-
    data %>%
    dplyr::mutate(s = total_num - sum(n),
                  row = dplyr::row_number(n))

  while(sum(data$s) != 0) {
    if (highlight_small) {
      data <-
        data %>%
        dplyr::mutate(n = ifelse(s != 0 & row == min(row),
                                 n + 1,
                                 n),
                      s = ifelse(s != 0,
                                 s - 1,
                                 s),
                      row = ifelse(row == min(row),
                                   row + total_num,
                                   row))
    } else {
      data <-
        data %>%
        dplyr::mutate(n = ifelse(s != 0 & row == max(row),
                                 n + 1,
                                 n),
                      s = ifelse(s != 0,
                                 s - 1,
                                 s),
                      row = ifelse(row == max(row),
                                   row - total_num,
                                   row))
    }
  }

  data %>%
    dplyr::select(-s, -row)
}


#' Create dataset for "tissue"-plotting
#'
#' @param data A tibble with a `n` column at least
#' @param thickness Tissue's width given in cell count
#' @param width Cell width
#' @param height Cell height
#' @return A tibble with coordinates of each cell within the tissue and corresponding value of variable of interest
create_coord_dataset <- function(data, thickness = 10, width = 4, height = 2.5) {
  total_num <-
    (data %>% dplyr::summarise(s = sum(n)))$s[1]
  data <-
    data %>%
    tidyr::uncount(n)

  data$x <- rep(sort(rep(1 : (total_num %/% thickness), thickness)), 2) * width
  data$y <- rep(rep(1:thickness, total_num %/% thickness), 2) * height

  return(data)
}
