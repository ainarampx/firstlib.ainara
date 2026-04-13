test_that("filtre_anomalie retire les anomalies et les totaux invalides", {
  df_test <- data.frame(
    check.names = FALSE,
    `Probabilité de présence d'anomalies` = c(NA, "Faible", NA, NA),
    Total = c(100, 200, -5, 15000),
    `Numéro de boucle` = c("880", "881", "882", "883")
  )

  res <- filtre_anomalie(df_test)

  expect_equal(nrow(res), 1)
  expect_equal(res$Total, 100)
})
