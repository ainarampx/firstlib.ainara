test_that("trouver_trajet_max renvoie le trajet ayant le total maximal", {
  df_test <- data.frame(
    check.names = FALSE,
    `Boucle de comptage` = c("A", "B", "C"),
    Jour = as.Date(c("2025-01-01", "2025-01-02", "2025-01-03")),
    Total = c(100, 250, 150)
  )

  res <- trouver_trajet_max(df_test)

  expect_equal(nrow(res), 1)
  expect_equal(res$Total, 250)
  expect_equal(res$`Boucle de comptage`, "B")
})
