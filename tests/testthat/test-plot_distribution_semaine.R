test_that("plot_distribution_semaine renvoie un objet ggplot", {
  df_test <- data.frame(
    check.names = FALSE,
    `Probabilité de présence d'anomalies` = c(NA, NA, NA),
    Total = c(10, 20, 30),
    `Jour de la semaine` = c(1, 2, 3)
  )

  res <- plot_distribution_semaine(df_test)

  expect_s3_class(res, "ggplot")
})
