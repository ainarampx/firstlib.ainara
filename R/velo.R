#' Filtrer les anomalies dans les trajets velo
#'
#' Cette fonction filtre un jeu de donnees de trajets velo afin de conserver
#' uniquement les observations sans anomalie detectee, avec un total strictement
#' positif et inferieur a 10000.
#'
#' @param trajet Un data.frame contenant les donnees de trajets velo.
#'
#' @return Un data.frame filtre.
#' @export
#'
#' @examples
#' filtre_anomalie(df_velo)
filtre_anomalie <- function(trajet) {
  trajet |>
    dplyr::filter(
      is.na(`Probabilité de présence d'anomalies`),
      Total < 10000,
      Total > 0
    )
}

#' Filtrer un jeu de donnees par numero de boucle
#'
#' Cette fonction permet de selectionner uniquement certaines boucles de
#' comptage dans un jeu de donnees de trajets velo.
#'
#' @param trajet Un data.frame contenant les donnees de trajets velo.
#' @param boucle Un vecteur de numeros de boucle a conserver.
#'
#' @return Un data.frame filtre sur les boucles selectionnees.
#' @export
#'
#' @examples
#' filtrer_trajet(trajet = df_velo, boucle = c("880", "881"))
filtrer_trajet <- function(trajet, boucle = NULL) {
  if (is.null(boucle)) {
    return(trajet)
  }

  trajet |>
    dplyr::filter(`Numéro de boucle` %in% boucle)
}


#' Compter le nombre total de trajets
#'
#' Cette fonction calcule la somme de la colonne \code{Total} d'un jeu de
#' donnees de trajets velo.
#'
#' @param trajet Un data.frame contenant les donnees de trajets velo.
#'
#' @return Un nombre correspondant au total des trajets.
#' @export
#'
#' @examples
#' compter_nombre_trajets(df_velo)
compter_nombre_trajets <- function(trajet) {
  trajet |>
    dplyr::pull(Total) |>
    sum(na.rm = TRUE)
}


#' Compter le nombre de boucles de comptage
#'
#' Cette fonction calcule le nombre de boucles de comptage distinctes presentes
#' dans un jeu de donnees.
#'
#' @param trajet Un data.frame contenant les donnees de trajets velo.
#'
#' @return Un entier correspondant au nombre de boucles distinctes.
#' @export
#'
#' @examples
#' compter_nombre_boucle(df_velo)
compter_nombre_boucle <- function(trajet) {
  trajet |>
    dplyr::pull(`Numéro de boucle`) |>
    dplyr::n_distinct()
}


#' Trouver le trajet avec le nombre maximal de passages
#'
#' Cette fonction renvoie la ou les observations pour lesquelles le nombre total
#' de trajets est maximal.
#'
#' @param trajet Un data.frame contenant les donnees de trajets velo.
#'
#' @return Un data.frame contenant la boucle de comptage, le jour et le total
#'   pour le ou les trajets maximaux.
#' @export
#'
#' @examples
#' trouver_trajet_max(df_velo)
trouver_trajet_max <- function(trajet) {
  trajet |>
    dplyr::slice_max(order_by = Total, n = 1, with_ties = TRUE) |>
    dplyr::select(`Boucle de comptage`, Jour, Total)
}


#' Calculer la distribution hebdomadaire des trajets
#'
#' Cette fonction calcule la repartition du nombre total de trajets selon le
#' jour de la semaine.
#'
#' @param trajet Un data.frame contenant les donnees de trajets velo.
#'
#' @return Un data.frame contenant les jours de la semaine et le nombre de
#'   trajets associe.
#' @export
#'
#' @examples
#' calcul_distribution_semaine(df_velo)
calcul_distribution_semaine <- function(trajet) {
  trajet |>
    dplyr::count(`Jour de la semaine`, wt = Total, sort = TRUE, name = "trajets")
}


#' Representer la distribution hebdomadaire des trajets
#'
#' Cette fonction produit un graphique en barres de la distribution du nombre
#' total de trajets selon le jour de la semaine, apres filtrage des anomalies.
#'
#' @param trajet Un data.frame contenant les donnees de trajets velo.
#'
#' @return Un graphique \code{ggplot2}.
#' @export
#'
#' @examples
#' plot_distribution_semaine(df_velo)
plot_distribution_semaine <- function(trajet) {
  trajet_weekday <- trajet |>
    filtre_anomalie() |>
    calcul_distribution_semaine() |>
    dplyr::mutate(
      jour = forcats::fct_recode(
        factor(`Jour de la semaine`),
        "lundi" = "1",
        "mardi" = "2",
        "mercredi" = "3",
        "jeudi" = "4",
        "vendredi" = "5",
        "samedi" = "6",
        "dimanche" = "7"
      )
    )

  ggplot2::ggplot(trajet_weekday) +
    ggplot2::aes(x = jour, y = trajets) +
    ggplot2::geom_col()
}
