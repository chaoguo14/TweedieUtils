tweedie_glmnet <- function(formula, df,
                           p = NULL, phi = NULL, prior_weights = rep(1, nrow(df)), offset = NULL,
                           lambda = NULL, alpha = NULL) {

  formula_str <- deparse(substitute(formula))

  X <- model.matrix(as.formula(formula_str), df)
  X <- X[, colnames(X) != "(Intercept)"]  # HDtweedie always adds the intercept term

  y_var_name <- all.vars(as.formula(formula_str))[1]
  y <- df[[y_var_name]]

  current_cv_error <- (2^31 - 1)
  best_alpha <- 2
  best_lambda <- 2

  alpha_seq <- c(0.0001, seq(0.25, 1, length.out = 9))
  cat(paste0(stringr::str_pad("Alpha", 10, "right"),
             stringr::str_pad("Best lambda", 20, "right"),
             stringr::str_pad("CV-MSE", 11, "right"), "\n"))
  cat(paste0(rep("=", 24)), "\n")

  for (alpha in alpha_seq) {
    cv_obj <- HDtweedie::cv.HDtweedie(x = X, y = y,
                                      p = p, weights = prior_weights, alpha = alpha, nlambda = 100,
                                      nfolds = 5, pred.loss = "mse")

    cat(paste0(stringr::str_pad(alpha, 10, "right"),
               stringr::str_pad(cv_obj$lambda.min, 20, "right"),
               stringr::str_pad(cv_obj$cvm[which(cv_obj$lambda == cv_obj$lambda.min)], 11, "right"),"\n"))

    if (cv_obj$cvm[which(cv_obj$lambda == cv_obj$lambda.min)] < current_cv_error) {
      best_alpha <- alpha
      best_lambda <- cv_obj$lambda.min
    }
  }

  optimal_model <- HDtweedie::HDtweedie(x = X, y = y,
                                        p = p, weights = prior_weights,
                                        alpha = alpha, lambda = rev(seq(best_lambda, by = 0.5, length.out = 50)))
  best_coef <- optimal_model$beta[, ncol(optimal_model$beta)]

  ret <- list(coef = best_coef,
              isSelected = (best_coef != 0))
  class(ret) <- "tweedie_glmnet"
  ret
}
