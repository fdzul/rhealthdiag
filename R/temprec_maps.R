#' Temperature and precipitation maps
#'
#' @param cve_mpo is the municipality id.
#' @param cve_ent is the state id.
#' @param locality is the name of locality.
#' @param raster_object is the precipitation or temperature raster object.
#' @param legend_title is the legend title.
#' @param width_legend is width of legend.
#'
#' @return a ggplot object.
#' @author felipe.dzul.@gmail.com
#' @export
#'
#' @examples
temprec_maps <- function(cve_mpo, cve_ent,
                          locality,
                          raster_object,
                          legend_title,
                          width_legend){

    # Step 1. extract the municipality ####
    x <- rgeomex::AGEM_inegi19_mx |>
        dplyr::filter(CVE_ENT %in% c(cve_ent)) |>
        dplyr::filter(CVE_MUN %in% c(cve_mpo)) |>
        sf::st_make_valid()

    # Step 7. extract the covariates ####
    extract_raster <- function(raster_object, sf_object ){
        terra::mask(x = terra::crop(x = raster_object,
                                    y = sf_object),
                    mask = sf_object)
    }

    y <- purrr::map(.x = raster_object,
                    .f = extract_raster,
                    sf_object = x)

    y <- raster::stack(y)

    names(y) <- c("Enero", "Febrero", "Marzo", "Abril",
                  "Mayo", "Junio", "Julio", "Agosto",
                  "Septiembre", "Octubre", "Noviembre",
                  "Diciembre")

    # convret to star object
    y <- stars::st_as_stars(y)
    loc <- rgeomex::extract_ageb(locality = locality,
                                 cve_geo =  cve_ent)
    ggplot2::ggplot() +
        stars::geom_stars(data = y) +
        ggplot2::geom_sf(data = loc$locality,
                         fill = "transparent") +
        ggplot2::facet_wrap("band") +
        viridis::scale_fill_viridis(legend_title) +
        #ggplot2::coord_equal() +
        cowplot::theme_map() +
        #ggthemes::theme_map() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::theme(legend.key.width = ggplot2::unit(width_legend, "cm"))
}
