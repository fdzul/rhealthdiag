#' climatic maps
#'
#' @param cve_ent is the state id.
#' @param cve_mpo is the municipality id.
#' @param state is logical value (TRUE or FALSE). If state is TRUE, the map is state leval, else the map is a municipality level.
#'
#' @return a ggplot.
#' @author felipe.dzul.m@gmail.com
#' @export
#'
#' @examples
clim_maps <- function(cve_ent,
                      cve_mpo = NULL,
                      state){

    if(state == TRUE)
    {
        x <- rgeomex::AGEE_inegi19_mx |>
            dplyr::filter(CVE_ENT %in% c(cve_ent))

    } else {
        x <- rgeomex::AGEM_inegi19_mx |>
            dplyr::filter(CVE_ENT %in% c(cve_ent)) |>
            dplyr::filter(CVE_MUN %in% c(cve_mpo))
    }


    clim <- rhealthdiag::climas[x, ]

    box <- sf::st_bbox(x)

    plotly::ggplotly(
        ggplot2::ggplot() +
            ggplot2::geom_sf(data = clim,
                             ggplot2::aes(fill = TIPO_C)) +
            ggplot2::scale_fill_brewer("Clima",
                                       palette = 2,
                                       direction = -1) +
            ggplot2::geom_sf(data = x,
                             fill = "transparent",
                             col = "blue") +
            ggplot2::coord_sf(xlim = c(box[1], box[3]),
                              ylim = c(box[2], box[4])) +
            cowplot::theme_map())
}
