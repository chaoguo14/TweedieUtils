tweedie_glm <- function(formula, df, p = NULL, phi = NULL, prior_weights = rep(1, nrow(df)), offset = NULL) {
  #' Fits a tweedie GLM model with log link.
  #'
  #' @description
  #' This function fits a tweedie GLM model with log link, power parameter p, dispersion parameter phi. User can also
  #' specify offset and weights. If power parameter p is not specified, it will be estimated before solving for coefficients using MLE.
  #' Parameter phi may be ignored since it does not affect estimation of coefficients. However, if it is provided, the provided
  #' value will be used when constructing confidence interval.
  #'
  #' @param formula Formula of the model
  #' @param df A data frame. Each column should be either numeric or factor. The response column should be non-negative and numeric
  #' @param p Power parameter of Tweedie distribution. It is supposed to be in [1, 2), but for numerical reason, it has to be in [1.1, 1.9]
  #' @param phi Dispersion parameter. It does not affect MLE of coefficients, but is useful in constructing confidence intervals.
  #' @param prior_weights The weights that appear in the score equation. For example, if the response is loss cost, then the prior_weights
  #' should be exposures. If the response is total loss, then the prior_weights should be $exposure^{p-1}$
  #' @param offset A numeric vector

  if (!is.data.frame(df)) {
    stop("df should be a data frame")
  }

  if (!is.null(p)) {
    if (p < 1.1 | p > 1.9) {
      stop("Power p should be between 1.1 and 1.9")
    }
  }

  formula_str <- deparse(substitute(formula))

  if (is.null(p)) {
    print("Estimating power parameter......")
    out <- tweedie::tweedie.profile(formula = as.formula(formula_str),
                           p.vec = seq(1.1, 1.8, by = 0.01),
                           link.power = 0,
                           data = df,
                           weights = prior_weights,
                           offset = offset,
                           do.smooth = TRUE)
    p <- out[['p']][which(out[['L']][is.finite(out[['L']])] == max(out[['L']][is.finite(out[['L']])]))]
  }


  model_obj <- glm(formula = as.formula(formula_str),
                   family = statmod::tweedie(var.power = p, link.power = 0),
                   data = df,
                   weights = prior_weights,
                   offset = offset)

  ret <- list(md = model_obj,
              coefficients = model_obj[['coefficients']],
              f = formula_str, p = p, phi = phi)
  class(ret) <- "tweedie_glm"
  ret
}

summary.tweedie_glm <- function(object, method = "Wald", ...) {
  #' Summarize a tweedie GLM
  #'
  #' @description
  #' This function summarizes a tweedie GLM. By default, it outputs the estimated coefficients and their standard error.
  #' For inference purpose, it also outputs 95% confidence intervals (Wald's test).
  #'
  #' @param object An S3 object returned by tweedie_glm()
  #' @param method Method to construct confidence interval. Currently only supports 'Wald', but might support 'LR' in the future.
  #' @param ... Other arguments

  phi <- object[["phi"]]
  if (is.null(phi)) {
    optimize_result <- optimize(f = function(phi) {tweedie::logLiktweedie(object[["md"]], dispersion = phi)},
                                lower = 0.0,
                                upper = 5000.0,
                                maximum = TRUE)
    phi <- optimize_result$maximum
  }

  s <- summary(object[["md"]], dispersion = phi)
  coef_tbl <- s$coefficients

  wald_chi_square <- (coef_tbl[, 1]**2)/(coef_tbl[, 2]**2)
  coef_tbl <- cbind(coef_tbl, wald_chi_square)
  coef_tbl[, 3] <- coef_tbl[, 1] - 1.96*coef_tbl[, 2]
  coef_tbl[, 4] <- coef_tbl[, 1] + 1.96*coef_tbl[, 2]
  coef_tbl <- cbind(coef_tbl, !((coef_tbl[, 3] < 0) & (coef_tbl[, 4] > 0)))

  coef_tbl <- as.data.frame(coef_tbl, row.names = rownames(coef_tbl))
  colnames(coef_tbl) <- c("Estimate", "Std. Error", "95% CI Lower Bound", "95% CI Upper Bound", "Wald Chi-Square", "Significant?")
  coef_tbl [, "Significant?"] <- ifelse(coef_tbl[, "Significant?"] == 1, "Yes", "No")

  cat(paste("\nModel Formula: ", object[['f']], "\n"))
  print(coef_tbl)

  cat("\nTECHNICAL NOTES:\n")
  if (method == "Wald") {
    cat(" - Using Wald test.\n")
  }
  cat(paste(" - Dispersion parameter phi", phi, "was estimated using MLE, unless supplied by the user.\n"))

  ret <- list(dispersion = phi,
              standard_error = coef_tbl[["Std. Error"]])
  class(ret) <- c("summary.tweedie_glm")
  ret
}
