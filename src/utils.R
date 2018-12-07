get_prediction_expectation <- function(predictions) {
  as_tibble(predictions) %>% 
    summarise_all(mean) %>% 
    t() %>% 
    as.numeric()
}

get_expectation_and_ci <- function(predictions, credible_interval = 0.89) {
  dplyr::as_tibble(predictions) %>% 
    dplyr::summarise_all(
      dplyr::funs(
        prediction = mean,
        lower_bound = round(rethinking::HPDI(.)[[1]], 2), # TODO add custom CI
        upper_bound = round(rethinking::HPDI(.)[[2]], 2))) %>%
    tidyr::gather() %>%
    dplyr::mutate(
      measurement_type = stringr::str_remove(.data$key, "V\\d*_"),
      data_point = stringr::str_match(.data$key, "\\d+")) %>%
    dplyr::select(-.data$key) %>%
    tidyr::spread(key = .data$measurement_type, value = .data$value) %>% 
    dplyr::mutate(data_point = as.numeric(.data$data_point)) %>% 
    dplyr::arrange(.data$data_point)
}