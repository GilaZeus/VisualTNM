#' Count missing values
#'
#' @param data A tibble
#' @param T Tumor size column name
#' @param N Nodal status column name
#' @param M Metastases column name
#' @param D Grading column name
#' @param U UICC column name
#' @return A tibble with three columns: full (which case of missing value), n (count), x (dummy for easy plotting)
check_integrity <- function(data, T, N, M, D, U) {
  T <- rlang::enquo(T)
  N <- rlang::enquo(N)
  M <- rlang::enquo(M)
  D <- rlang::enquo(D)
  U <- rlang::enquo(U)

  data %>%
    dplyr::mutate(full = ifelse(!is.na(!!T) & !is.na(!!N) & !is.na(!!M) & !is.na(!!D),
                         "1. Full",
                         NA)) %>%
    dplyr::mutate(full = ifelse(is.na(full) & !is.na(!!U) & (is.na(!!T) | is.na(!!N) | is.na(!!M)),
                         "2. Covered by UICC",
                         full)) %>%
    dplyr::mutate(full = ifelse(is.na(full) & !is.na(!!T) & !is.na(!!N) & !is.na(!!M) & is.na(!!D),
                         "3. Only D missing",
                         full)) %>%
    dplyr::mutate(full = ifelse(is.na(full) & is.na(!!T) & !is.na(!!N) & !is.na(!!M),
                         "4. One missing in TNM",
                         full)) %>%
    dplyr::mutate(full = ifelse(is.na(full) & !is.na(!!T) & is.na(!!N) & !is.na(!!M),
                         "4. One missing in TNM",
                         full)) %>%
    dplyr::mutate(full = ifelse(is.na(full) & !is.na(!!T) & !is.na(!!N) & is.na(!!M),
                         "4. One missing in TNM",
                         full)) %>%
    dplyr::count(full) %>%
    dplyr::mutate(x = "d",
                  full = ifelse(is.na(full),
                               "5. ≥ 2 missing values",
                               full))
}


#' Counts NAs.
#'
#' @param data A tibble
#' @param T Tumor size column name
#' @param N Nodal status column name
#' @param M Metastases column name
#' @param D Grading column name
#' @return A tibble with value, name, n columns containing number of NAs and whole data
count_NAs <- function(data, T, N, M, D) {
  T <- rlang::enquo(T)
  N <- rlang::enquo(N)
  M <- rlang::enquo(M)
  D <- rlang::enquo(D)

  data %>%
    dplyr::select(!!T, !!N, !!M, !!D) %>%
    dplyr::mutate(!!T := ifelse(is.na(!!T), NA, "T"),
                  !!N := ifelse(is.na(!!N), NA, "N"),
                  !!M := ifelse(is.na(!!M), NA, "M"),
                  !!D := ifelse(is.na(!!D), NA, "D")) %>%
    tidyr::pivot_longer(everything()) %>%
    dplyr::count(value, name) %>%
    dplyr::mutate(name = factor(name, levels = c("T", "N", "M", "D")))
}
