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
        lower_bound = round(rethinking::HPDI(., credible_interval)[[1]], 2),
        upper_bound = round(rethinking::HPDI(., credible_interval)[[2]], 2))) %>%
    tidyr::gather() %>%
    dplyr::mutate(
      measurement_type = stringr::str_remove(.data$key, "V\\d*_"),
      data_point = stringr::str_match(.data$key, "\\d+")) %>%
    dplyr::select(-.data$key) %>%
    tidyr::spread(key = .data$measurement_type, value = .data$value) %>% 
    dplyr::mutate(data_point = as.numeric(.data$data_point)) %>% 
    dplyr::arrange(.data$data_point)
}

plot_expectations <- function(data_points, expectations, x, y) {
  df <- dplyr::bind_cols(data_points, expectations)
  
  ggplot2::ggplot(df, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      ggplot2::aes_string(y = "prediction"), 
      color = "blue") +
    ggplot2::geom_ribbon(
      ggplot2::aes_string(
        ymin = "lower_bound", 
        ymax = "upper_bound"), 
      alpha = 0.5)
}

run_predictive_analysis <- function(model, 
                                    dataset, 
                                    controlling_variable = c(), 
                                    x, 
                                    y) {
  model_map <- rethinking::map(model, data = dataset)
  
  print(rethinking::precis(model_map))
  print(-rethinking::logLik(model_map))
  
  if (length(controlling_variable) > 0 ) {
    dataset <- dataset %>%
      dplyr::mutate_at(controlling_variable, mean)
  }
  
  predictions <- rethinking::link(model_map, dataset)
  expectations <- get_expectation_and_ci(predictions, 0.95)
  plot_expectations(dataset, expectations, x, y)
}