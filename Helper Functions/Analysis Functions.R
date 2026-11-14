# Fit logistic models to completed imputed data
fit_models <- function(completed_data, model_formula) {

  fit_models <- map(
    .x = completed_data,
    .f = ~ glm(data = .x, formula = model_formula, family = binomial)
  ) %>%
    as.mira()

  return(fit_models)

}

# Summarize models, exponentiating (i.e., log-odds to odds ratio) optionally
summarize_models <- function(fit_models, exp = T) {

  fit_models %>%
    pool() %>%
    summary(conf.int = T, exponentiate = exp) %>%
    kable(digits = 3) %>%
    kable_styling()

}

# Produce point predictions of self-labeling given years and severity levels
point_predictions <- function(
    fit_models,
    years,
    severity = c(5.5, 8.5, 12),
    moderator = NULL,
    moderator_values = unique,
    print = F
) {

  centered_years <- years - 2021

  grid_extra <- if(is.null(moderator)) list() else set_names(list(moderator_values), moderator)

  out <- rlang::inject(
    avg_predictions(
      !!fit_models,
      newdata = datagrid(
        depression_sum = !!severity,
        decimal_year_centered = !!centered_years,
        !!!grid_extra,
        grid_type = "counterfactual"),
      by = !!c("depression_sum", "decimal_year_centered", moderator)
    )
  )

  if(print) {

    out %>%
      as_tibble() %>%
      mutate(decimal_year = decimal_year_centered + 2021) %>%
      select(all_of(c(moderator, "depression_sum", "decimal_year", "estimate", "conf.low", "conf.high"))) %>%
      kable(digits = 3) %>%
      kable_styling() %>%
      return()

  } else return(out)

}

# Produce risk ratios at given severity levels
risk_ratios <- function(
    fit_models,
    variables_to_calculate = list(decimal_year_centered = 1),
    severity = c(5.5, 8.5, 12),
    moderator = NULL,
    moderator_values = unique,
    print = FALSE
) {

  grid_extra <- if (is.null(moderator)) list() else set_names(list(moderator_values), moderator)

  out <- rlang::inject(
    avg_comparisons(
      !!fit_models,
      variables = !!variables_to_calculate,
      comparison = "lnratioavg",
      transform = "exp",
      newdata = datagrid(depression_sum = !!severity, !!!grid_extra, grid_type = "counterfactual"),
      by = !!c("depression_sum", moderator)  # split by moderator when given
    )
  )

  if (print) {
    out %>%
      as_tibble() %>%
      select(all_of(c("term", "contrast", moderator, "depression_sum", "estimate", "conf.low", "conf.high", "p.value"))) %>%
      kable(digits = 3) %>%
      kable_styling()
  } else {
    out
  }

}

# First plot type: self-labeling by year by symptom severity (three levels)
plot_trend <- function(
    fit_models,
    severity = c(5.5, 8.5, 12),
    severity_labels = c("Low (DASS = 5.5)", "Medium (DASS = 8.5)", "High (DASS = 12)"),
    n_dates = 8
) {

  dates <- range(fit_models$analyses[[1]]$data$decimal_year)

  point_predictions(fit_models, years = seq(dates[1], dates[2], length.out = n_dates), severity = severity) %>%
    mutate(severity = factor(depression_sum, levels = severity, labels = severity_labels)) %>%
    ggplot(aes(x = decimal_year_centered + 2021, y = estimate, color = severity, fill = severity)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, color = NA) +
    geom_line() +
    scale_x_continuous(breaks = 2016:2026, expand = expansion(add = 0.1)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(x = NULL, y = "Predicted probability of self-labeling", color = NULL, fill = NULL) +
    theme_minimal() +
    theme(legend.position = "top")

}

# Second plot type: self-labeling by symptom severity by year (two levels)
plot_curve <- function(fit_models, severity = 0:21, years = c(2016.538, 2026.767)) {

  point_predictions(fit_models, years = years, severity = severity) %>%
    mutate(
      date_label = format(date_decimal(decimal_year_centered + 2021), "%B %Y"),
      date_label = fct_reorder(date_label, decimal_year_centered + 2021)
    ) %>%
    ggplot(aes(x = depression_sum, y = estimate, color = date_label, fill = date_label)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, color = NA) +
    geom_line() +
    scale_x_continuous(breaks = seq(0, 21, 3)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(x = "DASS-21 depression sum", y = "Predicted probability of self-labeling", color = NULL, fill = NULL) +
    theme_minimal() +
    theme(legend.position = "top")

}