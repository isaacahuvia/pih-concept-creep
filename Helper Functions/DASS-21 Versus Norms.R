# DASS-21 Depression norms for overlaying on a sample density plot
#
# Usage: source() this file, then call
#   res <- dass21_dep_compare(clean_data, depression_sum)
#   res <- dass21_dep_compare(clean_data, depression_sum, group = year)
# Column names can be given bare or in quotes. See section 4 for the optional arguments.
# All scores below are raw DASS-21 sums, 0-21 (multiply by 2 for the 0-42 metric).

# ---- 1. UK norms: Henry & Crawford (2005), Br J Clin Psychol 44:227-239 ---------------------
# UK non-clinical adults, N = 1,794, age 18-91; mean 2.83, SD 3.87, median 1.
#
# Their Table 2 is not a cumulative percent. The entry for a score of 0 is the percent
# scoring 0; entries for 1 and above are midpoint ranks (percent below plus half the percent
# at that score). That reading is inferred: it is the only one that reproduces the published
# mean, SD and median. pct and cum_pct are reconstructed from the table under that reading,
# constrained to match the published mean, SD, median and range.
#
# pr_published  percentile as printed in Table 2 (NA = printed as ">99")
# pct           estimated percent of the norm sample scoring exactly this score
# cum_pct       estimated percent scoring at or below this score
# cum_lo/cum_hi hard limits on cum_pct given everything published (not a confidence interval)

dass21_dep_norm <- data.frame(
  score        = 0:21,
  pr_published = c(34, 43, 58, 68, 75, 81, 85, 88, 91, 92, 93, 95, 96, 97, 97, 98, 98, 99,
                   NA, NA, NA, NA),
  pct          = c(34.50, 16.79, 12.43, 8.71, 6.16, 4.60, 3.58, 2.76, 1.96, 1.40, 1.22,
                   1.14, 0.96, 0.71, 0.55, 0.51, 0.50, 0.41, 0.24, 0.15, 0.26, 0.47),
  cum_lo       = c(33.5, 50.5, 61.5, 68.5, 75.5, 80.5, 84.5, 88.5, 90.5, 91.5, 92.5, 94.5,
                   95.5, 96.5, 96.5, 97.5, 97.5, 98.5, 99.0, 99.0, 99.0, 100),
  cum_hi       = c(34.5, 53.5, 66.5, 75.5, 81.5, 85.5, 88.5, 91.5, 92.5, 93.5, 95.5, 96.5,
                   97.5, 97.5, 98.5, 98.5, 99.5, 99.9, 99.9, 99.9, 99.9, 100)
)
dass21_dep_norm$pct     <- 100 * dass21_dep_norm$pct / sum(dass21_dep_norm$pct)
dass21_dep_norm$cum_pct <- cumsum(dass21_dep_norm$pct)

# ---- 2. US sample: Sinclair et al. (2012), Eval Health Prof 35:259-279 ----------------------
# US non-clinical adults matched to census targets, N = 499 analysed, age 18-87.
# The paper has NO percentile table. It publishes only: mean 5.70 and SD 8.20 (doubled),
# median 2 (doubled), 39.2% scoring 0, and 0.2% scoring the maximum.
#
# MODELLED, NOT PUBLISHED: pct below is the distribution closest in shape to the UK norms
# that matches those five US statistics. Only the first row (39.2% at zero) and the median
# are fixed by the paper; cum_lo/cum_hi show how little the rest is pinned down.

dass21_dep_us_modelled <- data.frame(
  score  = 0:21,
  pct    = c(39.20, 14.70, 10.95, 7.74, 5.55, 4.21, 3.34, 2.63, 1.93, 1.42, 1.27, 1.24,
             1.08, 0.84, 0.68, 0.66, 0.68, 0.59, 0.36, 0.25, 0.45, 0.20),
  cum_lo = c(39.0, 50.1, 50.1, 50.1, 58.0, 64.1, 68.1, 71.0, 73.1, 76.4, 81.2, 84.9, 87.6,
             89.8, 91.5, 92.7, 93.7, 94.6, 95.2, 95.8, 99.8, 100),
  cum_hi = c(39.4, 74.3, 80.7, 87.9, 95.0, 96.2, 96.8, 97.5, 98.2, 99.2, 99.8, 99.8, 99.8,
             99.8, 99.8, 99.8, 99.8, 99.8, 99.8, 99.8, 99.8, 100)
)
dass21_dep_us_modelled$pct     <- 100 * dass21_dep_us_modelled$pct / sum(dass21_dep_us_modelled$pct)
dass21_dep_us_modelled$cum_pct <- cumsum(dass21_dep_us_modelled$pct)

# ---- 3. Published summary statistics, all samples (raw 0-21 metric) -------------------------
# The manual sample used the 42-item DASS; its mean and SD are halved here.
# pct_zero for UK 2005 is the Table 2 entry for a score of 0.
dass21_dep_other <- data.frame(
  sample   = c("UK 2005, Henry & Crawford",
               "UK 2009 enlarged, Crawford et al.",
               "Australia 2011, Crawford et al.",
               "Australia 1995 manual, 42-item halved",
               "US 2012, Sinclair et al."),
  n        = c(1794, 2928, 497, 2914, 499),
  mean     = c(2.83, 3.18, 2.57, 3.17, 2.85),
  sd       = c(3.87, 4.16, 3.86, 3.49, 4.10),
  median   = c(1, 2, 1, NA, 1),
  pct_zero = c(34, NA, NA, NA, 39.2)
)

# ---- 4. Comparison function -----------------------------------------------------------------
# data             data frame with your DASS-21 Depression scores
# var              the Depression sum-score column (bare name or string)
# group            optional grouping column (bare name or string): one density curve per group
# k                1 = raw 0-21 sums, 2 = doubled 0-42. NULL guesses: doubled if any score > 21
# bw               density bandwidth on your metric; defaults to 1 raw point (k)
# show_us          include the modelled US 2012 curve
# show_percentiles dashed lines at the UK norm 50th, 75th, 90th and 95th percentiles
# show_bands       solid lines at the midpoints of the manual's mild, moderate and severe bands
# print            print the plot and summary table
#
# Returns (invisibly) a list:
#   plot     the ggplot object
#   summary  n, mean, SD, median and % scoring 0 for your sample (by group) and the published
#            samples, all on the raw 0-21 metric
#   data     your data with dep_norm_pct added: % of the UK norm sample scoring at or below
#            each person's score

dass21_dep_compare <- function(data, var, group = NULL, k = NULL, bw = NULL,
                               show_us = TRUE, show_percentiles = TRUE, show_bands = TRUE,
                               print = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Please install ggplot2.")

  # Accept bare column names, quoted names, or a variable holding a quoted name
  as_col <- function(expr, env) {
    if (is.null(expr)) return(NULL)
    if (is.character(expr) && length(expr) == 1) return(expr)
    if (is.name(expr)) {
      nm <- deparse(expr)
      if (nm %in% names(data)) return(nm)
      val <- tryCatch(eval(expr, env), error = function(e) NULL)
      if (is.character(val) && length(val) == 1) return(val)
      return(nm)
    }
    stop("Column names must be a bare name or a single string.")
  }
  var   <- as_col(substitute(var), parent.frame())
  group <- as_col(substitute(group), parent.frame())
  for (v in c(var, group)) if (!v %in% names(data)) stop("Column '", v, "' not found in data.")

  x <- data[[var]]
  if (!is.numeric(x)) stop("'", var, "' must be numeric.")
  if (is.null(k)) {
    k <- if (max(x, na.rm = TRUE) > 21) 2 else 1
    if (k == 2) message("Scores above 21 found: treating '", var, "' as doubled (0-42). Set k = 1 to override.")
  }
  if (!k %in% c(1, 2)) stop("k must be 1 (raw 0-21) or 2 (doubled 0-42).")
  if (max(x, na.rm = TRUE) > 21 * k) stop("Scores above ", 21 * k, " found; check k or the variable.")
  if (min(x, na.rm = TRUE) < 0) stop("Negative scores found in '", var, "'.")
  if (is.null(bw)) bw <- 1 * k

  # Your sample, with missing scores (and missing groups) dropped
  keep <- !is.na(x)
  if (!is.null(group)) {
    n_na_group <- sum(keep & is.na(data[[group]]))
    if (n_na_group > 0) message(n_na_group, " row(s) with a missing '", group, "' were left out of the plot and summary.")
    keep <- keep & !is.na(data[[group]])
  }
  samp <- data.frame(x = x[keep])
  samp$grp <- if (is.null(group)) factor("My sample") else factor(data[[group]][keep])

  # Summary table on the raw metric
  summ <- do.call(rbind, lapply(split(samp$x / k, samp$grp), function(xr)
    data.frame(n = length(xr), mean = mean(xr), sd = sd(xr), median = median(xr),
               pct_zero = 100 * mean(xr == 0))))
  summ <- data.frame(sample = if (is.null(group)) "My sample" else paste0("My sample: ", group, " = ", rownames(summ)),
                     summ, row.names = NULL)
  summ <- rbind(summ, dass21_dep_other)

  # Norm curves on your metric
  norm <- data.frame(dass21_dep_norm[c("score", "pct", "cum_pct")], source = "UK norms (2005)")
  if (show_us) norm <- rbind(norm, data.frame(dass21_dep_us_modelled[c("score", "pct", "cum_pct")],
                                              source = "US 2012 (modelled)"))
  norm$x <- norm$score * k
  norm$w <- norm$pct / 100
  uk <- norm[norm$source == "UK norms (2005)", ]

  norm_q   <- sapply(c(p50 = 50, p75 = 75, p90 = 90, p95 = 95), function(p) min(uk$x[uk$cum_pct >= p]))
  band_mid <- c(Mild = 5.5, Moderate = 8.5, Severe = 12) * k   # raw bands 5-6, 7-10, 11-13

  # Sample curves in colour, norm curves in grey with their own line types
  n_grp <- nlevels(samp$grp)
  pal <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#4a3aa7", "#e34948")
  grp_cols <- if (n_grp <= length(pal)) pal[seq_len(n_grp)] else grDevices::hcl.colors(n_grp, "Dark 3")

  p <- ggplot2::ggplot() +
    ggplot2::geom_density(data = samp, ggplot2::aes(x, colour = grp), bw = bw, linewidth = 0.8,
                          key_glyph = "path") +
    ggplot2::geom_density(data = norm, ggplot2::aes(x, weight = w, linetype = source),
                          bw = bw, colour = "grey35", linewidth = 0.6, key_glyph = "path") +
    ggplot2::scale_colour_manual(values = grp_cols, name = if (is.null(group)) NULL else group) +
    ggplot2::scale_linetype_manual(values = c("UK norms (2005)" = "solid", "US 2012 (modelled)" = "22"),
                                   name = NULL) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1),
                    linetype = ggplot2::guide_legend(order = 2)) +
    ggplot2::scale_x_continuous(limits = c(0, 21 * k), expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = paste0("DASS-21 Depression (", if (k == 1) "raw, 0-21" else "doubled, 0-42", ")"),
                  y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  if (show_percentiles) p <- p +
    ggplot2::geom_vline(xintercept = norm_q, linetype = "dashed", colour = "grey60") +
    ggplot2::annotate("text", x = norm_q, y = Inf, label = names(norm_q),
                      vjust = 1.5, hjust = 1.15, size = 3, colour = "grey45")
  if (show_bands) p <- p +
    ggplot2::geom_vline(xintercept = band_mid, linewidth = 0.4, colour = "grey25") +
    ggplot2::annotate("text", x = band_mid, y = Inf, label = names(band_mid),
                      vjust = 3.5, hjust = -0.1, size = 3, colour = "grey15")

  # Where each person falls in the UK norm distribution
  data$dep_norm_pct <- uk$cum_pct[match(x, uk$x)]

  if (print) {
    print(p)
    print(summ, digits = 3, row.names = FALSE)
  }
  invisible(list(plot = p, summary = summ, data = data))
}