#' Mortalidad by municipality/state, year, & age group.
#'
#' @param dataset is the mortality dataset.
#' @param years is the year or years analyzed.
#' @param cve_edo is the state id.
#' @param cve_mpo is the municipality id.
#' @param cat_group is the categorical age group. The options are "productiva", "infantil", "pediatric", "pos-productiva". The function provide the general mortality when cat_group is NULL.
#' @return a kabble object (table).
#' @export
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @examples
mort_year_mpo_dt <- function(dataset, years, cve_edo, cve_mpo, cat_group = NULL){

    if(is.null(cat_group) == TRUE){
        x <- dataset |>
            dplyr::filter(ent_ocurr %in% c(cve_edo))|>
            dplyr::filter(anio_ocur %in% c(years))|>
            dplyr::filter(mun_ocurr %in% c(cve_mpo)) |>
            dplyr::group_by(lista_mex_des,lista_mex_cve, anio_ocur)|>
            dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
            tidyr::pivot_wider(id_cols = c(lista_mex_des, lista_mex_cve),
                               names_from = anio_ocur,
                               values_from = n,
                               names_sort = TRUE,
                               values_fill = 0)
    } else{
        if(cat_group == "productiva"){
            vec <- c(unique(stringr::str_subset(dataset$edad_des, pattern = "Quince años|Dieciséis años|Dieciseis años|Diecisiete años|Dieciocho años|Diecinueve años")),
                     "Veinte años", "Veintiún años", "Veintiun años", "Veintidós años", "Veintidos años",
                     "Veintitrés años", "Veintitres años", "Veinticuatro años",
                     "Veinticinco años", "Veintiseis años", "Veintiséis años",
                     "Veintisiete años", "Veintiocho años", "Veintinueve años",
                     unique(stringr::str_subset(dataset$edad_des, pattern = "Treinta ")),
                     unique(stringr::str_subset(dataset$edad_des, pattern = "Cuarenta ")),
                     unique(stringr::str_subset(dataset$edad_des, pattern = "Cincuenta ")),
                     unique(stringr::str_subset(dataset$edad_des, pattern = "Sesenta ")))
        }
        if(cat_group == "post_productiva"){
            vec <- c(unique(stringr::str_subset(dataset$edad_des, pattern = "Setenta ")),
                     unique(stringr::str_subset(dataset$edad_des, pattern = "Ochenta ")),
                     unique(stringr::str_subset(dataset$edad_des, pattern = "Noventa ")),
                     unique(stringr::str_subset(dataset$edad_des, pattern = "Cien|Ciento")))
        }
        if(cat_group == "infantil"){
            vec <- c(unique(stringr::str_subset(dataset$edad_des, pattern = " horas| meses|Un ")))
        }
        if(cat_group == "pediatrica"){
            vec <- c(unique(stringr::str_subset(dataset$edad_des, pattern = "Dos |Tres |Cuatro |Cinco |Seis |Siete |Ocho |Nueve |Diez |Once |Doce |Trece |Catorce")))

        }



        x <- dataset |>
            dplyr::filter(ent_ocurr %in% c(cve_edo)) |>
            dplyr::filter(mun_ocurr %in% c(cve_mpo)) |>
            dplyr::filter(anio_ocur %in% c(years)) |>
            dplyr::filter(edad_des %in% c(vec)) |>
            dplyr::group_by(lista_mex_des, lista_mex_cve, anio_ocur) |>
            dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
            tidyr::pivot_wider(id_cols = c(lista_mex_des, lista_mex_cve),
                               names_from = anio_ocur,
                               values_from = n,
                               names_sort = TRUE,
                               values_fill = 0)
    }

    x$total <- x |>
        dplyr::select(dplyr::starts_with("20")) |>
        rowSums()
    x |>
        dplyr::arrange(dplyr::desc(total)) |>
        dplyr::rename("Causa de Defuncion" = lista_mex_des,
                      "Clave" = lista_mex_cve)|>
        kableExtra::kable() |>
        kableExtra::kable_styling() |>
        kableExtra:: scroll_box(width = "100%", height = "600px")
}
