#' New GTFS Mapper
#'
#' This function replaces the `map_gtfs()` with supported functionality in `leaflet`.
#'
#' @param gtfs.obj a GTFS object created by `gtfs_loader()`.
#' @param ag.name a string containing the name to be used in the legend. Defaults to `NULL`. If `NULL`, information will be drawn from the `agency` of `gtfs.obj`.
#' @param route.ids a vector of route ids to be mapped. Defaults to `NULL`. If `NULL`, all route ids will be used.
#' @param service.ids a vector of service ids to be mapped. See `??gtfsr::get_routes_sldf` for more information.
#' @param shape.ids a vector of shape ids to be mapped. See `??gtfsr::get_routes_sldf` for more information.
#' @param opacity a number between 0 and 1 to serve as the opacity value for routes on the map. Defaults to `0.7`.
#' @param route.colors a vector of colors to be used on the route. Defaults to `NULL`. If `NULL`, a default set of colors will be used. See `??gtfsr::get_routes_sldf` for more information.
#' @param verbose a boolean. If `TRUE`, will display status messages.
#' @param tileset a character naming the `leaflet` tileset to use. See `names(providers)` with leaflet attached for a list of values. If `NULL`, the default leaflet tileset will be used.
#' @param include.legend a boolean. If `TRUE`, a legend for routes will be displayed on the map output.
#'
#' @return A leaflet map object.
#' @export
#'
#' @examples
#'
gtfs_mapper <- function(gtfs.obj, ag.name = NULL, route.ids = NULL, service.ids = NULL, shape.ids = NULL, opacity = 0.7,
                        route.colors = NULL, verbose = FALSE, tileset = NULL, include.legend = TRUE) {
  if(is.null(ag.name)) {
    ag.name <- stringr::str_to_title(gtfs.obj$agency$agency_name)
    if(verbose) {
      mess <- paste("No agency name provided. Using ", ag.name, "...", sep = "")
      message(mess)
    }
  }

  if(is.null(route.ids)) {
    route.ids <- unique(gtfs.obj$routes$route_id)
  }

  if(!is.character(route.ids)) {
    route.ids <- as.character(route.ids)
  }
  plotting.data <- suppressMessages(prep_routes_for_map(gtfs.obj, route.ids, service.ids, shape.ids, opacity, route.colors))



  m <- leaflet::leaflet(plotting.data$gtfs_lines)
  if(is.null(tileset)) {
    m <- leaflet::addTiles(m)
  } else {
    m <- leaflet::addProviderTiles(m, tileset)
  }
  m <- m %>%
    leaflet::addPolylines(color = plotting.data$shapes_colors$color,
                          label = plotting.data$shapes_colors$labels,
                          group = plotting.data$shapes_colors$labels,
                          opacity = plotting.data$shapes_colors$opacity,
                          popup = plotting.data$shapes_colors$popups)
  if(include.legend) {
    m <- m %>% leaflet::addLegend(colors = plotting.data$routes_colors$color,
                                  labels = paste("Route", plotting.data$routes_colors$route_short_name),
                                  title = paste(ag.name, "Routes"))
  }

  overlays <- unique(plotting.data$shapes_colors$labels)
  m <- m %>% leaflet::addLayersControl(overlayGroups = overlays)
  return(m)
}
