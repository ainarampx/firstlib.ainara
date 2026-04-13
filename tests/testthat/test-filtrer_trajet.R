test_that("filtrer_trajet conserve seulement les boucles demandées", {
  df_test <- data.frame(
    check.names = FALSE,
    `Numéro de boucle` = c("880", "881", "900"),
    Total = c(10, 20, 30)
  )

  res <- filtrer_trajet(trajet = df_test, boucle = c("880", "881"))

  expect_equal(nrow(res), 2)
  expect_true(all(res$`Numéro de boucle` %in% c("880", "881")))
})

test_that("filtrer_trajet renvoie zéro ligne si aucune boucle ne correspond", {
  df_test <- data.frame(
    check.names = FALSE,
    `Numéro de boucle` = c("880", "881", "900"),
    Total = c(10, 20, 30)
  )

  res <- filtrer_trajet(trajet = df_test, boucle = c("999"))

  expect_equal(nrow(res), 0)
})

test_that("filtrer_trajet renvoie le jeu complet si boucle est NULL", {
  df_test <- data.frame(
    check.names = FALSE,
    `Numéro de boucle` = c("880", "881", "900"),
    Total = c(10, 20, 30)
  )

  res <- filtrer_trajet(trajet = df_test, boucle = NULL)

  expect_equal(res, df_test)
})
