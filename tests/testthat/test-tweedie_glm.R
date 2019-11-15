test_df_1 <- data.frame(Kilometres = c(1, 1, 1, 1, 1, 1, 5, 3, 5),
                        Bonus = c(1, 1, 1, 1, 1, 1, 7, 7, 7),
                        Make = as.factor(c(1, 2, 2, 4, 4, 8, 8, 8, 1)),
                        Insured = c(455.13, 69.17, 72.88, 1292.39, 191.01, 477.66, 13.06, 15.02, 384.87),
                        Payment = c(392491, 46221, 15694, 422201, 119373, 170913, 0, 14032, 112252))
test_df_1[['Make']] <- relevel(test_df_1[['Make']], ref = '8')
md_1 <- tweedie_glm(formula = Payment ~ Kilometres + Bonus + Make, df = test_df_1, offset = log(test_df_1$Insured))

test_that("Correct coefficient", {
  expect_equal(unname(md_1$coefficients),
               c(6.3986, -1.0429, 0.5100, 0.9007, 0.2130, 0.0531),
               tolerance = 0.00003)
})

test_that("Correct estimated power parameter", {
  expect_equal(md_1$p, 1.1)
})

test_that("Correct dispersion parameter estimate", {
  expect_equal(summary(md_1)$dispersion, 2172.615, tol = 0.00002, scale = 2172.615)
})

test_that("Correct estimated standard error", {
  expect_equal(summary(md_1)$standard_error,
               c(0.2240, 0.3627, 0.2273, 0.2499, 0.3760, 0.2481),
               tol = 0.03, scale = max(c(0.2240, 0.3627, 0.2273, 0.2499, 0.3760, 0.2481)))
})
