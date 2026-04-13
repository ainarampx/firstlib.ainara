test_that("calcul_distribution_semaine calcule les trajets par jour de la semaine", {
  df_test <- data.frame(
    check.names = FALSE,
    `Jour de la semaine` = c(1, 1, 2, 3),
    Total = c(10, 20, 30, 40)
  )

  res <- calcul_distribution_semaine(df_test, filtre = FALSE)

  expect_true("trajets" %in% names(res))
  expect_true(any(res$`Jour de la semaine` == 1))
  expect_equal(res$trajets[res$`Jour de la semaine` == 1], 30)
})

test_that("calcul_distribution_semaine ne filtre pas les données si filtre = FALSE", {
  df_test <- data.frame(
    check.names = FALSE,
    `Jour de la semaine` = c(1, 1, 2),
    Total = c(10, 20, 99999),
    `Probabilité de présence d'anomalies` = c(NA, NA, "Faible")
  )

  res <- calcul_distribution_semaine(df_test, filtre = FALSE)

  expect_equal(res$trajets[res$`Jour de la semaine` == 2], 99999)
})

test_that("calcul_distribution_semaine filtre les anomalies si filtre = TRUE", {
  df_test <- data.frame(
    check.names = FALSE,
    `Jour de la semaine` = c(1, 1, 2),
    Total = c(10, 20, 99999),
    `Probabilité de présence d'anomalies` = c(NA, NA, "Faible")
  )

  res <- calcul_distribution_semaine(df_test, filtre = TRUE)

  expect_false(any(res$trajets == 99999))
})
