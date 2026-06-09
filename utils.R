# utils.R
# Shared functions for Modern Analysis of Variance
# Source this file in a setup chunk at the top of every chapter:
#   source(here::here("utils.R"))

library(ggplot2)
library(patchwork)

anova_diagnostics <- function(fit, data) {
  
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("anova_diagnostics requires the car package. ",
         "Install it with: install.packages('car')")
  }
  
  # Extract residuals and fitted values
  res  <- residuals(fit)
  fitt <- fitted(fit)
  
  # Robustly extract the first grouping variable from the formula
  # Works for both one-way (y ~ A) and two-way (y ~ A * B) formulas
  rhs_vars <- all.vars(formula(fit)[[3]])
  grp      <- factor(data[[rhs_vars[1]]])
  
  diag_df <- data.frame(
    residuals  = res,
    fitted     = fitt,
    sqrt_abs_r = sqrt(abs(res)),
    group      = grp
  )
  
  # Plot 1: Residuals vs Fitted
  p1 <- ggplot(diag_df, aes(x = fitted, y = residuals)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    geom_point(colour = "#2E86AB", alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE,
                colour = "#E84855", linewidth = 0.8) +
    labs(title = "Residuals vs Fitted",
         x = "Fitted values", y = "Residuals") +
    theme_bw()
  
  # Plot 2: Q-Q plot
  p2 <- ggplot(diag_df, aes(sample = residuals)) +
    stat_qq(colour = "#2E86AB", alpha = 0.7) +
    stat_qq_line(colour = "#E84855", linewidth = 0.8) +
    labs(title = "Normal Q-Q",
         x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_bw()
  
  # Plot 3: Scale-Location
  p3 <- ggplot(diag_df, aes(x = fitted, y = sqrt_abs_r)) +
    geom_point(colour = "#2E86AB", alpha = 0.7) +
    geom_smooth(method = "loess", se = FALSE,
                colour = "#E84855", linewidth = 0.8) +
    labs(title = "Scale-Location",
         x = "Fitted values",
         y = expression(sqrt("|Residuals|"))) +
    theme_bw()
  
  # Plot 4: Residuals by first grouping variable
  p4 <- ggplot(diag_df, aes(x = group, y = residuals, fill = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    geom_boxplot(alpha = 0.6, outlier.shape = 21) +
    labs(title = paste("Residuals by", rhs_vars[1]),
         x = NULL, y = "Residuals") +
    theme_bw() +
    theme(legend.position = "none")
  
  print((p1 + p2) / (p3 + p4))
  
  # Formal tests
  cat("\n--- Shapiro-Wilk test (normality of residuals) ---\n")
  print(shapiro.test(res))
  
  cat("\n--- Levene's test (homogeneity of variance) ---\n")
  print(car::leveneTest(res ~ grp))
  
  # Summary guidance
  sw_p  <- shapiro.test(res)$p.value
  lev_p <- car::leveneTest(res ~ grp)$`Pr(>F)`[1]
  n_grp <- min(table(grp))
  
  cat("\n--- Diagnostic summary ---\n")
  cat("Sample size (smallest group):", n_grp, "\n")
  
  if (n_grp < 10) {
    cat("  Note: formal tests have low power with small samples.\n",
        "  Prioritise plots over p-values.\n")
  } else if (n_grp > 50) {
    cat("  Note: formal tests may flag trivial violations with large samples.\n",
        "  Assess practical significance from plots.\n")
  }
  
  cat("Shapiro-Wilk p =", round(sw_p, 4),
      ifelse(sw_p < 0.05,
             "-> Evidence of non-normality. Check Q-Q plot for outliers.\n",
             "-> No evidence against normality.\n"))
  
  cat("Levene's p =", round(lev_p, 4),
      ifelse(lev_p < 0.05,
             "-> Evidence of unequal variances. Consider Welch's ANOVA.\n",
             "-> No evidence against homoscedasticity.\n"))
}