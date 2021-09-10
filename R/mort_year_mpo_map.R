#' mortality maps
#'
#' @param dataset is the mortality dataset.
#' @param years is the year or years analyzed.
#' @param cve_edo is the state id.
#' @param cat_group is the categorical age group. The options are "productiva", "infantil", "pediatric", "pos-productiva". The function provide the general mortality when cat_group is NULL.
#' @param cve_def is the id of cause of mortality.
#' @param static_map is logical valur (TRUE o FALSE), if static_map is TRUE the map is static (ggplot2), else the maps es interecative (leafletmap)
#'
#' @return
#' @export
#'
#' @examples
mort_year_edo_map <- function(dataset, years,
                              cve_edo,
                              cat_group = NULL, cve_def = NULL,
                              static_map){

    if(is.null(cat_group) == TRUE){
        if(is.null(cve_def) == TRUE){
            x <- dataset |>
                dplyr::filter(ent_ocurr %in% c(cve_edo))|>
                dplyr::filter(anio_ocur %in% c(years)) |>
                dplyr::group_by(ent_ocurr, mun_ocurr, anio_ocur)|>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                tidyr::pivot_wider(id_cols = c(ent_ocurr, mun_ocurr),
                                   names_from = anio_ocur,
                                   values_from = n,
                                   names_sort = TRUE,
                                   values_fill = 0)
        } else {
            x <- dataset |>
                dplyr::filter(ent_ocurr %in% c(cve_edo))|>
                dplyr::filter(anio_ocur %in% c(years)) |>
                dplyr::filter(lista_mex_cve %in% c(cve_def)) |>
                dplyr::group_by(ent_ocurr, mun_ocurr, anio_ocur)|>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                tidyr::pivot_wider(id_cols = c(ent_ocurr, mun_ocurr),
                                   names_from = anio_ocur,
                                   values_from = n,
                                   names_sort = TRUE,
                                   values_fill = 0)
        }
    } else {
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

        if(is.null(cve_def) == TRUE){
            x <- dataset |>
                dplyr::filter(ent_ocurr %in% c(cve_edo)) |>
                dplyr::filter(anio_ocur %in% c(years)) |>
                dplyr::filter(edad_des %in% c(vec)) |>
                dplyr::group_by(ent_ocurr, mun_ocurr,
                                anio_ocur) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                tidyr::pivot_wider(id_cols = c(ent_ocurr, mun_ocurr),
                                   names_from = anio_ocur,
                                   values_from = n,
                                   names_sort = TRUE,
                                   values_fill = 0)


        } else{
            x <- dataset |>
                dplyr::filter(ent_ocurr %in% c(cve_edo))|>
                dplyr::filter(anio_ocur %in% c(years))|>
                dplyr::filter(edad_des %in% c(vec)) |>
                dplyr::filter(lista_mex_cve %in% c(cve_def)) |>
                dplyr::group_by(ent_ocurr, mun_ocurr, anio_ocur)|>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                tidyr::pivot_wider(id_cols = c(ent_ocurr, mun_ocurr),
                                   names_from = anio_ocur,
                                   values_from = n,
                                   names_sort = TRUE,
                                   values_fill = 0)
        }

    }



    xy <- dplyr::left_join(x = rgeomex::AGEM_inegi19_mx |>
                               dplyr::filter(CVE_ENT %in% c(cve_edo)),
                           y = x,
                           by = c("CVE_ENT" = "ent_ocurr",
                                  "CVE_MUN" = "mun_ocurr")) %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("20"))

    if(static_map == TRUE){
        plotly::ggplotly(
            ggplot2::ggplot() +
                ggplot2::geom_sf(data = xy,
                                 ggplot2::aes(fill = value,
                                              label = NOMGEO),
                                 col = "white",
                                 lwd = 0.01) +
                ggplot2::scale_fill_viridis_c() +
                cowplot::theme_map())

    } else{
        # leaflet options ####
        pal <- leaflet::colorNumeric(palette = fishualize::fish(n = length(unique(xy$value)),
                                                                option = "Scarus_hoefleri",
                                                                direction = -1),
                                     domain = xy$value,
                                     reverse = FALSE)



        #########
        xy$labels <- paste0("<strong> clave mpo: </strong> ",
                            xy$CVEGEO, "<br/> ",
                            "<strong> nombre mpo: </strong> ",
                            xy$NOMGEO, "<br/> ",
                            "<strong> defunciones: </strong> ",
                            xy$value, "<br/> ") |>
            lapply(htmltools::HTML)

        l <- leaflet::leaflet(data =  xy) |>
            leaflet::addTiles() |>
            leaflet::addPolygons(group = "defunciones",
                                 color = "white",
                                 weight = 1,
                                 fillColor = ~pal(value),
                                 fillOpacity = .5,
                                 label = ~labels,
                                 highlightOptions = leaflet::highlightOptions(color = "black",
                                                                              bringToFront = TRUE)) |>
            leaflet::addLegend(position = "topleft",
                               pal = pal,
                               values = ~value,
                               title = "Defunciones",
                               opacity = 1)

        esri <- grep("^Esri|CartoDB|OpenStreetMap", leaflet::providers, value = TRUE)
        for (provider in esri) {
            l <- l %>% leaflet::addProviderTiles(provider,
                                                 group = provider)
        }

        l %>%
            leaflet::addLayersControl(baseGroups = names(esri),
                                      options = leaflet::layersControlOptions(collapsed = TRUE),
                                      overlayGroups = c("defunciones")) %>%
            leaflet::addMiniMap(tiles = esri[[1]],
                                toggleDisplay = TRUE,
                                minimized = TRUE,
                                position = "bottomleft") %>%
            htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
    }
}

