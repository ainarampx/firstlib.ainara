test_that("compter_nombre_boucle compte les boucles distinctes", {
  df_test <- data.frame(
    check.names = FALSE,
    `Numéro de boucle` = c("880", "881", "880", "900")
  )

  res <- compter_nombre_boucle(df_test)

  expect_equal(res, 3)
})
