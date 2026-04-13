test_that("compter_nombre_trajets calcule correctement la somme des totaux", {
  df_test <- data.frame(
    check.names = FALSE,
    Total = c(10, 20, 30, NA)
  )

  res <- compter_nombre_trajets(df_test)

  expect_equal(res, 60)
})
