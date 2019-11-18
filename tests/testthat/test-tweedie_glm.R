test_df_1 <- data.frame(Kilometres = c(1, 1, 1, 1, 1, 1, 5, 3, 5),
                        Bonus = c(1, 1, 1, 1, 1, 1, 7, 7, 7),
                        Make = as.factor(c(1, 2, 2, 4, 4, 8, 8, 8, 1)),
                        Insured = c(455.13, 69.17, 72.88, 1292.39, 191.01, 477.66, 13.06, 15.02, 384.87),
                        Payment = c(392491, 46221, 15694, 422201, 119373, 170913, 0, 14032, 112252))
test_df_1[['Make']] <- relevel(test_df_1[['Make']], ref = '8')
model_1 <- tweedie_glm(formula = Payment~Kilometres + Bonus + Make, df = test_df_1, offset = log(test_df_1$Insured))

test_df_2 <- read.csv("test_data_2.csv", colClasses = c(rep("factor", 5), "numeric", "numeric"))
test_df_2$class_a <- relevel(test_df_2$class_a, ref = "3")
test_df_2$class_b <- relevel(test_df_2$class_b, ref = "3")
test_df_2$class_c <- relevel(test_df_2$class_c, ref = "3")
test_df_2$class_d <- relevel(test_df_2$class_d, ref = "3")
test_df_2$class_e <- relevel(test_df_2$class_e, ref = "3")
model_2 <- tweedie_glm(formula = y~class_a+class_b+class_c+class_d+class_e, df = test_df_2)

test_that("Correct coefficient", {
  true_coef_1 <- c(6.3986, -1.0429, 0.5100, 0.9007, 0.2130, 0.0531)
  expect_true(max(abs((model_1$coefficients - true_coef_1)/true_coef_1)) < 0.025)

  true_coef_2 <- c(3.888904, -0.072400, -1.358456, 0.154711, 1.350591, 1.159242, 0.033921, -0.217763, -0.289425, -0.131961, -0.258069, -0.057042, 0.219697, -1.314657, -0.996980, -0.481185)
  expect_true(max(abs((model_2$coefficients - true_coef_2)/true_coef_2)) < 0.025)
})

test_that("Correct estimated power parameter", {
  expect_equal(model_1$p, 1.1)
})

test_that("Correct dispersion parameter estimate", {
  expect_true(abs((summary(model_1)$dispersion - 2172.615)/2172.615) < 0.025)
})

test_that("Correct estimated standard error", {
  true_se_1 <- c(0.2240, 0.3627, 0.2273, 0.2499, 0.3760, 0.2481)
  expect_true(max(abs((summary(model_1)$standard_error - true_se_1)/true_se_1)) < 0.05)
})
