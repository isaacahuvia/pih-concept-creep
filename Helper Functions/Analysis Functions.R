####  Summarize  ####
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



####  Model  ####
# Fitted models are large (about 45 MB per imputed dataset), so functions below keep results, not model objects

# Fit logistic models to completed imputed data
fit_models <- function(completed_data, model_formula) {

  model_formula <- as.formula(model_formula)

  fit_models <- map(
    .x = completed_data,
    .f = ~ do.call( # do.call() stores the data itself in each call, so marginaleffects always recovers the right data
      "glm",
      list(
        formula = model_formula,
        data = select(.x, all_of(all.vars(model_formula))), # Keep only the model's variables (smaller models and prediction grids)
        family = binomial)
      )
  ) %>%
    as.mira()

  return(fit_models)

}

# Pool estimates across imputed datasets with Rubin's rules, within groups defined by `by`
# Takes a list (one element per imputed dataset) of tables with estimate and std.error; with one dataset, nothing is pooled
pool_rubin <- function(estimates, by) {

  estimates %>%
    bind_rows() %>%
    summarize(
      m = n(),
      variance_between = if (n() > 1) var(estimate) else 0,
      variance_within = mean(std.error^2),
      estimate = mean(estimate),
      .by = all_of(by)
    ) %>%
    mutate(
      std.error = sqrt(variance_within + (1 + 1 / m) * variance_between),
      df = if_else(variance_between > 0, (m - 1) * (1 + variance_within / ((1 + 1 / m) * variance_between))^2, Inf),
      p.value = 2 * pt(abs(estimate / std.error), df, lower.tail = F),
      conf.low = estimate - qt(.975, df) * std.error,
      conf.high = estimate + qt(.975, df) * std.error
    ) %>%
    as_tibble()

}

# Pooled coefficients (log-odds), p-values, and 95% CIs
pooled_coefficients <- function(fit_models) {

  map(
    fit_models$analyses,
    ~ summary(.x)$coefficients %>%
      as_tibble(rownames = "term") %>%
      select(term, estimate = Estimate, std.error = `Std. Error`)
  ) %>%
    pool_rubin("term") %>%
    select(term, estimate, p.value, conf.low, conf.high)

}

# Convert log-odds to odds ratios, per `unit` of the predictor
odds_ratios <- function(coefficients, unit = 1) {

  coefficients %>%
    mutate(across(c(estimate, conf.low, conf.high), ~ exp(.x * unit)))

}

# Print a results table
show_table <- function(results, digits = 3) {

  results %>%
    kable(digits = digits) %>%
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
    moderator_values = NULL
) {

  grid_extra <- moderator_grid(fit_models, moderator, moderator_values)
  by <- c("depression_sum", if (!is.null(years)) "decimal_year_centered", moderator)

  # One severity level and year at a time: the counterfactual grid copies the data for every combination,
  # so this keeps memory use small
  points <- if (is.null(years)) map(severity, ~ list(depression_sum = .x)) else
    pmap(expand_grid(depression_sum = severity, decimal_year_centered = years - 2021), list)

  # Predictions in each imputed dataset, then pooled
  # data.frame() drops the marginaleffects attribute, which holds the whole prediction grid (about 150 MB)
  map(fit_models$analyses, function(fit) {
    map(points, ~ rlang::inject(
      avg_predictions(
        fit,
        newdata = datagrid(!!!.x, !!!grid_extra, grid_type = "counterfactual"),
        by = !!by
      )
    ) %>%
      data.frame()) %>%
      bind_rows()
  }) %>%
    pool_rubin(by) %>%
    mutate(across(any_of("decimal_year_centered"), ~ .x + 2021, .names = "decimal_year")) %>%
    select(any_of(c(moderator, "depression_sum", "decimal_year", "estimate", "conf.low", "conf.high")))

}

# Produce risk ratios at given severity levels
risk_ratios <- function(
    fit_models,
    variables_to_calculate = list(decimal_year_centered = 1),
    severity = c(5.5, 8.5, 12),
    moderator = NULL,
    moderator_values = NULL
) {

  grid_extra <- moderator_grid(fit_models, moderator, moderator_values)
  by <- c("term", "contrast", "depression_sum", moderator)

  # Log risk ratios in each imputed dataset (data.frame() as in point_predictions()), pooled, then exponentiated
  map(
    fit_models$analyses,
    ~ rlang::inject(
      avg_comparisons(
        .x,
        variables = !!variables_to_calculate,
        comparison = "lnratioavg",
        newdata = datagrid(depression_sum = !!severity, !!!grid_extra, grid_type = "counterfactual"),
        by = !!c("depression_sum", moderator)
      )
    ) %>%
      data.frame()
  ) %>%
    pool_rubin(by) %>%
    mutate(across(c(estimate, conf.low, conf.high), exp)) %>%
    select(all_of(c("term", "contrast", moderator, "depression_sum", "estimate", "conf.low", "conf.high", "p.value")))

}

# Calculate mean BIC across imputed datasets
mean_bic <- function(fit_models) map_dbl(fit_models$analyses, BIC) %>% mean()

# Plot data use the first five imputations only (run time); pooled with Rubin's rules as elsewhere, so plotted
# points can differ slightly from the tables, which use all imputations
plot_imputations <- function(fit_models, m = 5) as.mira(head(fit_models$analyses, m))

# Fit a model and keep only its results: pooled coefficients, point predictions (if years are given),
# risk ratios per year, and (if plots = T) data for plot_trend() and plot_curve()
summarize_fit <- function(
    completed_data,
    model_formula,
    years = NULL,
    severity = c(5.5, 8.5, 12),
    moderator = NULL,
    moderator_values = NULL,
    plots = F
) {

  fit <- fit_models(completed_data, model_formula)
  dates <- range(fit$analyses[[1]]$data$decimal_year_centered) + 2021 # First and last observed dates

  list(
    mean_bic = mean_bic(fit),
    coefficients = pooled_coefficients(fit),
    predictions = if (!is.null(years)) point_predictions(fit, years, severity, moderator, moderator_values),
    ratios = risk_ratios(fit, severity = severity, moderator = moderator, moderator_values = moderator_values),
    trend = if (plots) point_predictions(plot_imputations(fit), seq(dates[1], dates[2], length.out = 8), c(5.5, 8.5, 12), moderator, moderator_values),
    curve = if (plots) point_predictions(plot_imputations(fit), dates, 0:21, moderator, moderator_values)
  )

}

# Fit a model in which the year trend is moderated by a covariate; keep the moderator x year odds ratios
# (per `unit` of a numeric moderator), risk ratios per year by moderator value, and optionally plot data
fit_moderation <- function(completed_data, base_formula, moderator, moderator_values = NULL, unit = 1, severity = 8.5, plots = F) {

  moderated_formula <- update(as.formula(base_formula), as.formula(". ~ . + " %+% moderator %+% " * decimal_year_centered"))

  results <- summarize_fit(completed_data, moderated_formula, severity = severity, moderator = moderator, moderator_values = moderator_values, plots = plots)

  results$coefficients <- results$coefficients %>%
    filter(
      str_detect(term, "decimal_year_centered:|:decimal_year_centered"),
      str_detect(term, fixed(moderator)),
      !str_detect(term, "dataset")
    ) %>%
    odds_ratios(unit) %>%
    mutate(moderator = moderator, unit = unit, .before = 1)

  results$ratios <- results$ratios %>%
    mutate(value = as.character(.data[[moderator]]), .after = contrast) %>%
    select(-all_of(moderator))

  results

}

# Fit a model adding a covariate to a base model; keep the covariate's odds ratios (per unit for numeric covariates),
# the risk ratio between covariate levels (factors: each level vs. reference; numeric: highest vs. lowest value),
# and data for plot_curve_by()
fit_secondary <- function(completed_data, base_formula, covariate, values = NULL, unit = 1, severity = 8.5) {

  full_model <- fit_models(completed_data, base_formula %+% " + " %+% covariate)

  list(
    coefficients = pooled_coefficients(full_model) %>%
      filter(str_starts(term, fixed(covariate))) %>%
      odds_ratios(unit) %>%
      mutate(covariate = covariate, unit = unit, .before = 1),
    ratios = risk_ratios(
      full_model,
      variables_to_calculate = set_names(list(if (is.null(values)) "reference" else range(values)), covariate),
      severity = severity
    ),
    curve = point_predictions(plot_imputations(full_model), years = NULL, severity = 0:21, moderator = covariate, moderator_values = values)
  )

}

# Fit and compare candidate model specifications on fit (mean BIC), point predictions, and risk ratios
# model_formulas is a named list of formulas; the first is the reference for all differences
# Returns (invisibly) these tables plus pooled coefficients (log-odds) for each model
compare_models <- function(
    completed_data,
    model_formulas,
    years = c(2016, 2026),
    severity = c(5.5, 8.5, 12),
    print = T
) {

  model_names <- names(model_formulas)

  # Fit each model in turn, keeping only its results
  results <- map(model_formulas, ~ summarize_fit(completed_data, .x, years = years, severity = severity))

  stack <- function(part) {
    map(results, part) %>%
      bind_rows(.id = "model") %>%
      mutate(model = fct(model, levels = model_names))
  }

  # Fit: mean BIC, and change relative to the reference model (negative = better fit)
  fit <- tibble(
    model = fct(model_names, levels = model_names),
    mean_bic = map_dbl(results, "mean_bic")
  ) %>%
    mutate(delta_bic = mean_bic - first(mean_bic))

  # Point predictions, and change relative to the reference model (in probability units)
  predictions <- stack("predictions") %>%
    mutate(difference = estimate - first(estimate), .by = c(depression_sum, decimal_year)) %>%
    arrange(depression_sum, decimal_year, model)

  # Risk ratios per year, and change relative to the reference model
  ratios <- stack("ratios") %>%
    select(model, depression_sum, estimate, conf.low, conf.high) %>%
    mutate(difference = estimate - first(estimate), .by = depression_sum) %>%
    arrange(depression_sum, model)

  out <- list(fit = fit, predictions = predictions, ratios = ratios, coefficients = stack("coefficients"))

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



####  Plot  ####
# These take prediction data from summarize_fit(), fit_moderation(), or fit_secondary()
# Prediction data with a `group` column (see label_groups()) are drawn one panel or color per group

# Label groups (moderator or covariate values) for plots: value labels if given, else the values themselves
label_groups <- function(predictions, variable, values = NULL, value_labels = NULL) {

  predictions %>%
    mutate(group = if (is.null(value_labels)) factor(.data[[variable]]) else factor(.data[[variable]], levels = values, labels = value_labels))

}

# First plot type: self-labeling by year by symptom severity (three levels)
plot_trend <- function(
    predictions,
    severity = c(5.5, 8.5, 12),
    severity_labels = c("Low (DASS = 5.5)", "Medium (DASS = 8.5)", "High (DASS = 12)"),
    title = NULL
) {

  grouped <- "group" %in% names(predictions)

  p <- predictions %>%
    mutate(severity = factor(depression_sum, levels = severity, labels = severity_labels)) %>%
    ggplot(aes(x = decimal_year, y = estimate, color = severity, fill = severity)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, color = NA) +
    geom_line() +
    scale_x_continuous(breaks = seq(2016, 2026, if (grouped) 2 else 1), expand = expansion(add = 0.1)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(x = NULL, y = "Predicted probability of self-labeling", color = NULL, fill = NULL, title = title) +
    theme_minimal() +
    theme(legend.position = "top")

  if (grouped) p + facet_wrap(~ group) else p

}

# Second plot type: self-labeling by symptom severity by year (first and last observed dates)
plot_curve <- function(predictions, title = NULL) {

  p <- predictions %>%
    mutate(
      date_label = format(date_decimal(decimal_year), "%B %Y"),
      date_label = fct_reorder(date_label, decimal_year)
    ) %>%
    ggplot(aes(x = depression_sum, y = estimate, color = date_label, fill = date_label)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, color = NA) +
    geom_line() +
    scale_x_continuous(breaks = seq(0, 21, 3)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(x = "DASS-21 depression sum", y = "Predicted probability of self-labeling", color = NULL, fill = NULL, title = title) +
    theme_minimal() +
    theme(legend.position = "top")

  if ("group" %in% names(predictions)) p + facet_wrap(~ group) else p

}

# Third plot type: self-labeling by symptom severity, one color per group (for models without year)
plot_curve_by <- function(predictions, label = NULL) {

  predictions %>%
    ggplot(aes(x = depression_sum, y = estimate, color = group, fill = group)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, color = NA) +
    geom_line() +
    scale_x_continuous(breaks = seq(0, 21, 3)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
    labs(x = "DASS-21 Depression Sum Score", y = "Predicted Probability of Self-Labeling", color = label, fill = label) +
    theme_minimal() +
    theme(legend.position = "top")

}
