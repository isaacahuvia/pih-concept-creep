####  Summarize data  ####
# Summarize a categorical variable by dataset as n (%), with %s excluding missing values
# Cells with no observations (i.e., levels not assessed in a dataset) are shown as "—"
summarize_categorical <- function(data, var, label) {

  level_rows <- data %>%
    filter(!is.na(.data[[var]])) %>%
    count(dataset, level = .data[[var]], .drop = FALSE) %>%
    mutate(
      pct = n / sum(n),
      value = if_else(n == 0, "\u2014", comma(n) %+% " (" %+% percent(pct, accuracy = .1) %+% ")"),
      .by = dataset
    ) %>%
    select(dataset, row = level, value) %>%
    mutate(row = as.character(row)) %>%
    pivot_wider(names_from = dataset, values_from = value) %>%
    mutate(is_header = FALSE)

  header_row <- tibble(row = label, is_header = TRUE)

  bind_rows(header_row, level_rows)

}

# Summarize a continuous variable by dataset as mean (SD), formatted to the given accuracy
summarize_continuous <- function(data, var, label, accuracy, prefix = "") {

  fmt <- function(x) number(x, accuracy = accuracy, prefix = prefix, big.mark = ",")

  data %>%
    summarize(
      m = mean(.data[[var]], na.rm = T),
      s = sd(.data[[var]], na.rm = T),
      .by = dataset
    ) %>%
    mutate(value = fmt(m) %+% " (" %+% fmt(s) %+% ")") %>%
    select(dataset, value) %>%
    pivot_wider(names_from = dataset, values_from = value) %>%
    mutate(row = label, is_header = FALSE)

}



####  Work with models  ####
# Fit logistic models to completed imputed data
fit_models <- function(completed_data, model_formula) {

  fit_models <- map(
    .x = completed_data,
    .f = ~ glm(data = .x, formula = model_formula, family = binomial)
  ) %>%
    as.mira()

  return(fit_models)

}

# Fit a model in which the year trend is moderated by a covariate, and test the moderation
# The test is a pooled Wald test (D1) of all moderator x year terms, against a model with the
# moderator's main effect only
fit_moderation <- function(completed_data, base_formula, moderator) {

  base_formula <- as.formula(base_formula)
  null_formula <- update(base_formula, as.formula(". ~ . + " %+% moderator))
  moderated_formula <- update(base_formula, as.formula(". ~ . + " %+% moderator %+% " * decimal_year_centered"))

  null_model <- fit_models(completed_data, null_formula)
  moderated_model <- fit_models(completed_data, moderated_formula)

  test <- D1(moderated_model, null_model)$result

  list(
    model = moderated_model,
    test = tibble(
      moderator = moderator,
      f = test[, "F.value"],
      df1 = test[, "df1"],
      df2 = test[, "df2"],
      p.value = test[, "P(>F)"]
    )
  )

}

# Fit a model adding a covariate to a base model, and test the covariate
# The test is a pooled Wald test (D1) of all of the covariate's terms, against the base model
fit_secondary <- function(completed_data, base_formula, covariate) {

  base_formula <- as.formula(base_formula)
  full_formula <- update(base_formula, as.formula(". ~ . + " %+% covariate))

  base_model <- fit_models(completed_data, base_formula)
  full_model <- fit_models(completed_data, full_formula)

  test <- D1(full_model, base_model)$result

  list(
    model = full_model,
    test = tibble(
      covariate = covariate,
      f = test[, "F.value"],
      df1 = test[, "df1"],
      df2 = test[, "df2"],
      p.value = test[, "P(>F)"]
    )
  )

}

# Summarize models, exponentiating (i.e., log-odds to odds ratio) optionally
summarize_models <- function(fit_models, exp = T) {

  fit_models %>%
    pool() %>%
    summary(conf.int = T, exponentiate = exp) %>%
    kable(digits = 3) %>%
    kable_styling()

}

# Build the moderator part of a prediction grid
# Factors default to every level; numeric moderators require explicit values
moderator_grid <- function(fit_models, moderator, moderator_values) {

  if(is.null(moderator)) return(list())

  if(is.null(moderator_values)) {
    if (is.numeric(fit_models$analyses[[1]]$data[[moderator]])) {
      stop("Numeric moderator `", moderator, "` needs moderator_values.")
    }
    moderator_values <- unique # Factors: every level
  }

  set_names(list(moderator_values), moderator)

}

# Produce point predictions of self-labeling given severity levels, and optionally years
# Set years = NULL for models without a year term
point_predictions <- function(
    fit_models,
    years,
    severity = c(5.5, 8.5, 12),
    moderator = NULL,
    moderator_values = NULL,
    print = F
) {

  year_grid <- if (is.null(years)) list() else list(decimal_year_centered = years - 2021)
  grid_extra <- moderator_grid(fit_models, moderator, moderator_values)

  out <- rlang::inject(
    avg_predictions(
      !!fit_models,
      newdata = datagrid(
        depression_sum = !!severity,
        !!!year_grid,
        !!!grid_extra,
        grid_type = "counterfactual"),
      by = !!c("depression_sum", names(year_grid), moderator)
    )
  )

  if (print) {
    out %>%
      as_tibble() %>%
      mutate(across(any_of("decimal_year_centered"), ~ .x + 2021, .names = "decimal_year")) %>%
      select(any_of(c(moderator, "depression_sum", "decimal_year", "estimate", "conf.low", "conf.high"))) %>%
      kable(digits = 3) %>%
      kable_styling()
  } else {
    out
  }

}

# Produce risk ratios at given severity levels
risk_ratios <- function(
    fit_models,
    variables_to_calculate = list(decimal_year_centered = 1),
    severity = c(5.5, 8.5, 12),
    moderator = NULL,
    moderator_values = NULL,
    print = F
) {

  grid_extra <- moderator_grid(fit_models, moderator, moderator_values)

  out <- rlang::inject(
    avg_comparisons(
      !!fit_models,
      variables = !!variables_to_calculate,
      comparison = "lnratioavg",
      transform = "exp",
      newdata = datagrid(depression_sum = !!severity, !!!grid_extra, grid_type = "counterfactual"),
      by = !!c("depression_sum", moderator)
    )
  )

  if(print) {
    out %>%
      as_tibble() %>%
      select(all_of(c("term", "contrast", moderator, "depression_sum", "estimate", "conf.low", "conf.high", "p.value"))) %>%
      kable(digits = 3) %>%
      kable_styling()
  } else {
    out
  }

}

# Calculate mean BIC across imputed datasets
mean_bic <- function(fit_models) map_dbl(fit_models$analyses, BIC) %>% mean()

# Fit and compare candidate model specifications on fit (mean BIC), point predictions, and risk ratios
# model_formulas is a named list of formulas; the first is the reference for all differences
compare_models <- function(
    completed_data,
    model_formulas,
    years = c(2016, 2026),
    severity = c(5.5, 8.5, 12),
    print = T
) {

  # Fit each model to every completed dataset
  models <- map(model_formulas, ~ fit_models(completed_data, .x))
  model_names <- names(model_formulas)

  # Fit: mean BIC, and change relative to the reference model (negative = better fit)
  fit <- models %>%
    map_dbl(mean_bic) %>%
    enframe(name = "model", value = "mean_bic") %>%
    mutate(
      model = fct(model, levels = model_names),
      delta_bic = mean_bic - first(mean_bic)
    )

  # Point predictions, and change relative to the reference model (in probability units)
  predictions <- models %>%
    map(~ point_predictions(.x, years = years, severity = severity) %>% as_tibble()) %>%
    bind_rows(.id = "model") %>%
    transmute(
      model = fct(model, levels = model_names),
      depression_sum,
      decimal_year = decimal_year_centered + 2021,
      estimate, conf.low, conf.high
    ) %>%
    mutate(difference = estimate - first(estimate), .by = c(depression_sum, decimal_year)) %>%
    arrange(depression_sum, decimal_year, model)

  # Risk ratios per year, and change relative to the reference model
  ratios <- models %>%
    map(~ risk_ratios(.x, severity = severity) %>% as_tibble()) %>%
    bind_rows(.id = "model") %>%
    transmute(
      model = fct(model, levels = model_names),
      depression_sum,
      estimate, conf.low, conf.high
    ) %>%
    mutate(difference = estimate - first(estimate), .by = depression_sum) %>%
    arrange(depression_sum, model)

  out <- list(models = models, fit = fit, predictions = predictions, ratios = ratios)

  if (print) {
    cat("\nModel fit (mean BIC across imputations; delta relative to " %+% model_names[1] %+% ")\n")
    print(fit)
    cat("\nPoint predictions (difference relative to " %+% model_names[1] %+% ")\n")
    print(predictions, n = Inf)
    cat("\nRisk ratios per year (difference relative to " %+% model_names[1] %+% ")\n")
    print(ratios, n = Inf)
  }

  invisible(out)

}



####  Plot trends  ####
# First plot type: self-labeling by year by symptom severity (three levels), optionally by moderator
plot_trend <- function(
    fit_models,
    severity = c(5.5, 8.5, 12),
    severity_labels = c("Low (DASS = 5.5)", "Medium (DASS = 8.5)", "High (DASS = 12)"),
    n_dates = 8,
    moderator = NULL,
    moderator_values = NULL
) {

  dates <- range(fit_models$analyses[[1]]$data$decimal_year)

  p <- point_predictions(
    fit_models,
    years = seq(dates[1], dates[2], length.out = n_dates),
    severity = severity,
    moderator = moderator,
    moderator_values = moderator_values
  ) %>%
    as_tibble() %>%
    mutate(severity = factor(depression_sum, levels = severity, labels = severity_labels)) %>%
    ggplot(aes(x = decimal_year_centered + 2021, y = estimate, color = severity, fill = severity)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, color = NA) +
    geom_line() +
    scale_x_continuous(breaks = 2016:2026, expand = expansion(add = 0.1)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(x = NULL, y = "Predicted probability of self-labeling", color = NULL, fill = NULL) +
    theme_minimal() +
    theme(legend.position = "top")

  if (is.null(moderator)) return(p)

  p + facet_wrap(~ .data[[moderator]], labeller = as_labeller(\(x) moderator %+% " = " %+% x))

}

# Second plot type: self-labeling by symptom severity by year (two levels), optionally by moderator
# Years default to the first and last observed dates in the model data
plot_curve <- function(
    fit_models,
    severity = 0:21,
    years = NULL,
    moderator = NULL,
    moderator_values = NULL
) {

  if (is.null(years)) years <- range(fit_models$analyses[[1]]$data$decimal_year)

  p <- point_predictions(
    fit_models,
    years = years,
    severity = severity,
    moderator = moderator,
    moderator_values = moderator_values
  ) %>%
    as_tibble() %>%
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

  if (is.null(moderator)) return(p)

  p + facet_wrap(~ .data[[moderator]], labeller = as_labeller(\(x) moderator %+% " = " %+% x))

}

# Third plot type: self-labeling by symptom severity, by levels of a covariate (for models without year)
plot_curve_by <- function(fit_models, variable, values = NULL, severity = 0:21) {

  point_predictions(fit_models, years = NULL, severity = severity, moderator = variable, moderator_values = values) %>%
    as_tibble() %>%
    mutate(group = factor(.data[[variable]])) %>%
    ggplot(aes(x = depression_sum, y = estimate, color = group, fill = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, color = NA) +
    geom_line() +
    scale_x_continuous(breaks = seq(0, 21, 3)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(x = "DASS-21 depression sum", y = "Predicted probability of self-labeling", color = variable, fill = variable) +
    theme_minimal() +
    theme(legend.position = "top")

}