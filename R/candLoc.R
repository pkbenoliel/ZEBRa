#' Candidate Location Searcher
#'
#' This function returns the top number of times a stop is stopped at in a network. It should be noted that the total number of times a bus is scheduled to stop at a bus is all that is considered.
#'
#' @param gtfs.obj a GTFS object, preferably generated by the function gtfs_loader
#' @param num.stops the number of stops to display on the output. This value is not used if `all.routes = TRUE`
#' @param syn.network a vector of routes as a synthetic network, or NULL if the entire network is to be used
#' @param all.routes a boolean. If set to `TRUE`, candidate locations are selected such that every route has at least one location to charge at (this ignores `num.stops`). If `FALSE`, only the top locations are selected regardless of if routes are left out.
#' @param ag.name character, the title of the legend in the map. Optional; if blank, the information will be pulled from `agency`.
#' @param opacity numeric, the opacity value of the routes (higher numbers are darker)
#' @param verbose a boolean. If `TRUE`, diagnostic information will be displayed.
#' @param include.legend a boolean. If `TRUE`, a legend for routes will be displayed on the map output.
#' @param tileset a character naming the `leaflet` tileset to use. See `names(providers)` with leaflet attached for a list of values. If `NULL`, the default leaflet tileset will be used.
#' @param include.depots a boolean. If `TRUE`, depots will be included as red pins if depot data is available.
#'
#' @return a list containing a histogram of the stops, a table of stops, a map of the routes and stops, and the list of stops at each location.
#' @export
#'
#' @examples
#'
cand_loc_searcher <- function(gtfs.obj, num.stops = 10, syn.network = NULL, all.routes = TRUE, ag.name = NULL,
                              opacity = 0.7, verbose = TRUE, include.legend = TRUE, tileset = NULL,
                              include.depots = TRUE, include.map = TRUE) {
  stop.times <- gtfs.obj$stop_times
  stops <- gtfs.obj$stops
  if(!is.null(syn.network)) {
    trips <- gtfs.obj$trips %>% dplyr::select(c("trip_id", "route_id"))
    stop.times <- base::merge(stop.times, trips, by = "trip_id")
    stop.times <- stop.times[stop.times$route_id %in% syn.network,]
  }
  if(!all.routes) {
    stops.frequency <- as.data.frame(dplyr::count(stop.times, stop_id, sort = TRUE, name = "occ"))[c(1:num.stops),]

  } else {
    unchecked.stops <- as.data.frame(dplyr::count(stop.times, stop_id, sort = TRUE, name = "occ"))
    unserved.routes <- gtfs.obj$routes$route_id
    if(!is.null(syn.network)) {
      unserved.routes <- unserved.routes[unserved.routes %in% syn.network]
    }
    stops.frequency <- data.frame()
    for(i in 1:nrow(unchecked.stops)) {
      stop.tmp <- unchecked.stops$stop_id[i]
      routes.tmp <- stop_on_routes(gtfs.obj, stop.tmp)
      if(!any(unserved.routes %in% routes.tmp)) {

      } else {
        new.routes.served <- unserved.routes[unserved.routes %in% routes.tmp]
        stops.frequency <- rbind(stops.frequency, unchecked.stops[i,])
        unserved.routes <- unserved.routes[!unserved.routes %in% new.routes.served]
      }
    }
  }
  stops.frequency$label <- NA
  for(i in 1:nrow(stops.frequency)) {
    stops.frequency$label[i] <- stops$stop_name[stops$stop_id == stops.frequency$stop_id[i]]
  }
  stops.frequency$lat <- 0
  stops.frequency$lon <- 0
  for(i in 1:nrow(stops.frequency)) {
    stops.frequency$lat[i] <- stops$stop_lat[stops$stop_id == stops.frequency$stop_id[i]]
    stops.frequency$lon[i] <- stops$stop_lon[stops$stop_id == stops.frequency$stop_id[i]]
  }

  stops.frequency$group <- NA
  stops.frequency$group[1] <- "1"
  let.tmp <- 1
  if(nrow(stops.frequency) > 1) {
    for(i in 2:nrow(stops.frequency)) {
      for(j in 1:(i-1)) {
        dist.check <- latlon_2_meter(stops.frequency$lat[i], stops.frequency$lon[i], stops.frequency$lat[j], stops.frequency$lon[j])
        if(dist.check <= 100) {
          if(is.na(stops.frequency$group[i])) {
            stops.frequency$group[i] <- stops.frequency$group[j]
          } else if(stops.frequency$group[j] %in% stops.frequency$group[i]) {

          } else {
            stops.frequency$group[i] <- paste(stops.frequency$group[i], stops.frequency$group[j], sep = ", ")
          }
        }
      }
      if(is.na(stops.frequency$group[i])) {
        let.tmp <- let.tmp + 1
        stops.frequency$group[i] <- as.character(let.tmp)
      }
    }
  }

  the.list <- list()
  for(i in 1:let.tmp) {
    ids.tmp <- numeric()
    for(j in 1:nrow(stops.frequency)) {
      if(as.character(i) %in% stops.frequency$group[j]) {
        ids.tmp <- c(ids.tmp, stops.frequency$stop_id[j])
      }
    }
    the.list[[i]] <- ids.tmp
  }

  names(the.list) <- as.character(seq(from = 1, to = let.tmp, by = 1))
  the.plot <- ggplot2::ggplot(data = stops.frequency, ggplot2::aes(x = label, y = occ)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7 ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if(!("shape_id" %in% names(gtfs.obj$trips)) | !("shapes" %in% names(gtfs.obj))) {
    if(verbose) {
      message("\nShape IDs missing. Skipping mapping.")
    }
    the.map <- NULL
  } else if(!include.map){
    the.map <- NULL
  } else {
    ids.to.map <- NULL
    for(i in 1:length(the.list)){
      ids.to.map <- c(ids.to.map, as.character(the.list[[i]][1]))
    }
    lats.to.map <- stops.frequency$lat[stops.frequency$stop_id %in% ids.to.map]
    lons.to.map <- stops.frequency$lon[stops.frequency$stop_id %in% ids.to.map]
    the.map <- gtfs_mapper(gtfs.obj, ag.name = ag.name, route.ids = syn.network, opacity = opacity, verbose = verbose, tileset = tileset, include.legend = include.legend)
    blueIcon <- leaflet::makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
      iconWidth = 25,
      iconHeight = 41,
      iconAnchorX = 12,
      iconAnchorY = 41,
      shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
      shadowHeight = 41,
      shadowWidth = 41
    )
    the.map <- the.map %>% leaflet::addMarkers(lng = lons.to.map, lat = lats.to.map, icon = blueIcon, label = names(the.list))
  }
  return.list <- list(stops_plot = the.plot, stops_table = stops.frequency, map = the.map, stops_list = the.list)
  return(return.list)
}

cand_loc_mode <- function(gtfs.obj, mode.number, charger.number, syn.network = NULL) {
  switch(mode.number,
         {output <- cand_loc_searcher_allRoutes(gtfs.obj)},
         {output <- cand_loc_searcher_opportunities(gtfs.obj, num.stops = charger.number, syn.network = syn.network)},
         {output <- cand_loc_searcher_freq(gtfs.obj, num.stops = charger.number, syn.network = syn.network)},
         {output <- cand_loc_searcher_miles(gtfs.obj, num.stops = charger.number, syn.network = syn.network)},
         {stop("Mode not recognized.")}
         )
  return(output)
}

## ----------------------|
## MODE 1: ALL ROUTES    |
## ----------------------|

cand_loc_searcher_allRoutes <- function(gtfs.obj, syn.network = NULL, exclude.table = NULL) {
  stop.times <- gtfs.obj$stop_times
  stops <- gtfs.obj$stops


  unchecked.stops <- as.data.frame(dplyr::count(stop.times, stop_id, sort = TRUE, name = "occ"))
  unserved.routes <- gtfs.obj$routes$route_id
  if(!is.null(syn.network)) {
    unserved.routes <- unserved.routes[unserved.routes %in% syn.network]
  }
  stops.frequency <- data.frame()
  for(i in 1:nrow(unchecked.stops)) {
    stop.tmp <- unchecked.stops$stop_id[i]
    routes.tmp <- stop_on_routes(gtfs.obj, stop.tmp)
    if(!any(unserved.routes %in% routes.tmp)) {

    } else {
      new.routes.served <- unserved.routes[unserved.routes %in% routes.tmp]
      stops.frequency <- rbind(stops.frequency, unchecked.stops[i,])
      unserved.routes <- unserved.routes[!unserved.routes %in% new.routes.served]
    }
  }
  stops.frequency$label <- NA
  for(i in 1:nrow(stops.frequency)) {
    stops.frequency$label[i] <- stops$stop_name[stops$stop_id == stops.frequency$stop_id[i]]
  }
  stops.frequency$lat <- 0
  stops.frequency$lon <- 0
  for(i in 1:nrow(stops.frequency)) {
    stops.frequency$lat[i] <- stops$stop_lat[stops$stop_id == stops.frequency$stop_id[i]]
    stops.frequency$lon[i] <- stops$stop_lon[stops$stop_id == stops.frequency$stop_id[i]]
  }

  stops.frequency$group <- NA
  stops.frequency$group[1] <- "1"
  let.tmp <- 1
  if(nrow(stops.frequency) > 1) {
    for(i in 2:nrow(stops.frequency)) {
      for(j in 1:(i-1)) {
        dist.check <- latlon_2_meter(stops.frequency$lat[i], stops.frequency$lon[i], stops.frequency$lat[j], stops.frequency$lon[j])
        if(dist.check <= 100) {
          if(is.na(stops.frequency$group[i])) {
            stops.frequency$group[i] <- stops.frequency$group[j]
          } else if(stops.frequency$group[j] %in% stops.frequency$group[i]) {

          } else {
            stops.frequency$group[i] <- paste(stops.frequency$group[i], stops.frequency$group[j], sep = ", ")
          }
        }
      }
      if(is.na(stops.frequency$group[i])) {
        let.tmp <- let.tmp + 1
        stops.frequency$group[i] <- as.character(let.tmp)
      }
    }
  }

  the.list <- list()
  for(i in 1:let.tmp) {
    ids.tmp <- numeric()
    for(j in 1:nrow(stops.frequency)) {
      if(as.character(i) %in% stops.frequency$group[j]) {
        ids.tmp <- c(ids.tmp, stops.frequency$stop_id[j])
      }
    }
    the.list[[i]] <- ids.tmp
  }

  names(the.list) <- as.character(seq(from = 1, to = let.tmp, by = 1))
  the.plot <- ggplot2::ggplot(data = stops.frequency, ggplot2::aes(x = label, y = occ)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7 ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if(!("shape_id" %in% names(gtfs.obj$trips)) | !("shapes" %in% names(gtfs.obj))) {
    stop("Mapping could not be completed due to missing shape information.")
  } else {
    ids.to.map <- NULL
    for(i in 1:length(the.list)){
      ids.to.map <- c(ids.to.map, as.character(the.list[[i]][1]))
    }
    lats.to.map <- stops.frequency$lat[stops.frequency$stop_id %in% ids.to.map]
    lons.to.map <- stops.frequency$lon[stops.frequency$stop_id %in% ids.to.map]
    the.map <- gtfs_mapper(gtfs.obj, ag.name = NULL, route.ids = syn.network, opacity = 0.7, verbose = FALSE, tileset = NULL, include.legend = FALSE)
    blueIcon <- leaflet::makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
      iconWidth = 25,
      iconHeight = 41,
      iconAnchorX = 12,
      iconAnchorY = 41,
      shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
      shadowHeight = 41,
      shadowWidth = 41
    )
    the.map <- the.map %>% leaflet::addMarkers(lng = lons.to.map, lat = lats.to.map, icon = blueIcon, label = names(the.list))
  }
  return.list <- list(stops_plot = the.plot, stops_table = stops.frequency, map = the.map, stops_list = the.list)
  return(return.list)
}

## ----------------------------------|
## MODE 2: CHARGING OPPORTUNITIES    |
## ----------------------------------|

cand_loc_searcher_opportunities <- function(gtfs.obj, num.stops, syn.network = NULL, exclude.table = NULL) {
  stop.times <- gtfs.obj$stop_times
  stops <- gtfs.obj$stops
  if(!is.null(syn.network)) {
    trips <- gtfs.obj$trips %>% dplyr::select(c("trip_id", "route_id"))
    stop.times <- base::merge(stop.times, trips, by = "trip_id")
    stop.times <- stop.times[stop.times$route_id %in% syn.network,]
  }
  stops.frequency <- as.data.frame(dplyr::count(stop.times, stop_id, sort = TRUE, name = "occ"))[c(1:num.stops),]
  stops.frequency$label <- NA
  for(i in 1:nrow(stops.frequency)) {
    stops.frequency$label[i] <- stops$stop_name[stops$stop_id == stops.frequency$stop_id[i]]
  }
  stops.frequency$lat <- 0
  stops.frequency$lon <- 0
  for(i in 1:nrow(stops.frequency)) {
    stops.frequency$lat[i] <- stops$stop_lat[stops$stop_id == stops.frequency$stop_id[i]]
    stops.frequency$lon[i] <- stops$stop_lon[stops$stop_id == stops.frequency$stop_id[i]]
  }

  stops.frequency$group <- NA
  stops.frequency$group[1] <- "1"
  let.tmp <- 1
  if(nrow(stops.frequency) > 1) {
    for(i in 2:nrow(stops.frequency)) {
      for(j in 1:(i-1)) {
        dist.check <- latlon_2_meter(stops.frequency$lat[i], stops.frequency$lon[i], stops.frequency$lat[j], stops.frequency$lon[j])
        if(dist.check <= 100) {
          if(is.na(stops.frequency$group[i])) {
            stops.frequency$group[i] <- stops.frequency$group[j]
          } else if(stops.frequency$group[j] %in% stops.frequency$group[i]) {

          } else {
            stops.frequency$group[i] <- paste(stops.frequency$group[i], stops.frequency$group[j], sep = ", ")
          }
        }
      }
      if(is.na(stops.frequency$group[i])) {
        let.tmp <- let.tmp + 1
        stops.frequency$group[i] <- as.character(let.tmp)
      }
    }
  }

  the.list <- list()
  for(i in 1:let.tmp) {
    ids.tmp <- numeric()
    for(j in 1:nrow(stops.frequency)) {
      if(as.character(i) %in% stops.frequency$group[j]) {
        ids.tmp <- c(ids.tmp, stops.frequency$stop_id[j])
      }
    }
    the.list[[i]] <- ids.tmp
  }

  names(the.list) <- as.character(seq(from = 1, to = let.tmp, by = 1))
  the.plot <- ggplot2::ggplot(data = stops.frequency, ggplot2::aes(x = label, y = occ)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7 ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if(!("shape_id" %in% names(gtfs.obj$trips)) | !("shapes" %in% names(gtfs.obj))) {
    stop("Mapping could not be completed due to missing shape information.")
  } else {
    ids.to.map <- NULL
    for(i in 1:length(the.list)){
      ids.to.map <- c(ids.to.map, as.character(the.list[[i]][1]))
    }
    lats.to.map <- stops.frequency$lat[stops.frequency$stop_id %in% ids.to.map]
    lons.to.map <- stops.frequency$lon[stops.frequency$stop_id %in% ids.to.map]
    the.map <- gtfs_mapper(gtfs.obj, ag.name = NULL, route.ids = syn.network, opacity = 0.7, verbose = FALSE, tileset = NULL, include.legend = FALSE)
    blueIcon <- leaflet::makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
      iconWidth = 25,
      iconHeight = 41,
      iconAnchorX = 12,
      iconAnchorY = 41,
      shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
      shadowHeight = 41,
      shadowWidth = 41
    )
    the.map <- the.map %>% leaflet::addMarkers(lng = lons.to.map, lat = lats.to.map, icon = blueIcon, label = names(the.list))
  }
  return.list <- list(stops_plot = the.plot, stops_table = stops.frequency, map = the.map, stops_list = the.list)
  return(return.list)
}

## ---------------------------|
## MODE 3: LONGEST ROUTES     |
## ---------------------------|

cand_loc_searcher_miles <- function(gtfs.obj, num.stops, syn.network = NULL, exclude.table = NULL) {
  stop.times <- gtfs.obj$stop_times
  stops <- gtfs.obj$stops
  if(!is.null(syn.network)) {
    trips <- gtfs.obj$trips %>% dplyr::select(c("trip_id", "route_id"))
    stop.times <- base::merge(stop.times, trips, by = "trip_id")
    stop.times <- stop.times[stop.times$route_id %in% syn.network,]
  }
  stops.frequency <- as.data.frame(dplyr::count(stop.times, stop_id, sort = TRUE, name = "occ"))[c(1:num.stops),]
  stops.frequency$label <- NA
  for(i in 1:nrow(stops.frequency)) {
    stops.frequency$label[i] <- stops$stop_name[stops$stop_id == stops.frequency$stop_id[i]]
  }
  stops.frequency$lat <- 0
  stops.frequency$lon <- 0
  for(i in 1:nrow(stops.frequency)) {
    stops.frequency$lat[i] <- stops$stop_lat[stops$stop_id == stops.frequency$stop_id[i]]
    stops.frequency$lon[i] <- stops$stop_lon[stops$stop_id == stops.frequency$stop_id[i]]
  }

  stops.frequency$group <- NA
  stops.frequency$group[1] <- "1"
  let.tmp <- 1
  if(nrow(stops.frequency) > 1) {
    for(i in 2:nrow(stops.frequency)) {
      for(j in 1:(i-1)) {
        dist.check <- latlon_2_meter(stops.frequency$lat[i], stops.frequency$lon[i], stops.frequency$lat[j], stops.frequency$lon[j])
        if(dist.check <= 100) {
          if(is.na(stops.frequency$group[i])) {
            stops.frequency$group[i] <- stops.frequency$group[j]
          } else if(stops.frequency$group[j] %in% stops.frequency$group[i]) {

          } else {
            stops.frequency$group[i] <- paste(stops.frequency$group[i], stops.frequency$group[j], sep = ", ")
          }
        }
      }
      if(is.na(stops.frequency$group[i])) {
        let.tmp <- let.tmp + 1
        stops.frequency$group[i] <- as.character(let.tmp)
      }
    }
  }

  the.list <- list()
  for(i in 1:let.tmp) {
    ids.tmp <- numeric()
    for(j in 1:nrow(stops.frequency)) {
      if(as.character(i) %in% stops.frequency$group[j]) {
        ids.tmp <- c(ids.tmp, stops.frequency$stop_id[j])
      }
    }
    the.list[[i]] <- ids.tmp
  }

  names(the.list) <- as.character(seq(from = 1, to = let.tmp, by = 1))
  the.plot <- ggplot2::ggplot(data = stops.frequency, ggplot2::aes(x = label, y = occ)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7 ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if(!("shape_id" %in% names(gtfs.obj$trips)) | !("shapes" %in% names(gtfs.obj))) {
    stop("Mapping could not be completed due to missing shape information.")
  } else {
    ids.to.map <- NULL
    for(i in 1:length(the.list)){
      ids.to.map <- c(ids.to.map, as.character(the.list[[i]][1]))
    }
    lats.to.map <- stops.frequency$lat[stops.frequency$stop_id %in% ids.to.map]
    lons.to.map <- stops.frequency$lon[stops.frequency$stop_id %in% ids.to.map]
    the.map <- gtfs_mapper(gtfs.obj, ag.name = NULL, route.ids = syn.network, opacity = 0.7, verbose = FALSE, tileset = NULL, include.legend = FALSE)
    blueIcon <- leaflet::makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
      iconWidth = 25,
      iconHeight = 41,
      iconAnchorX = 12,
      iconAnchorY = 41,
      shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
      shadowHeight = 41,
      shadowWidth = 41
    )
    the.map <- the.map %>% leaflet::addMarkers(lng = lons.to.map, lat = lats.to.map, icon = blueIcon, label = names(the.list))
  }
  return.list <- list(stops_plot = the.plot, stops_table = stops.frequency, map = the.map, stops_list = the.list)
  return(return.list)
}

## ------------------------------|
## MODE 4: MOST FREQUENT ROUTES  |
## ------------------------------|

cand_loc_searcher_freq <- function(gtfs.obj, num.stops, syn.network = NULL, exclude.table = NULL, freq.table) {
  stop.times <- gtfs.obj$stop_times
  stops <- gtfs.obj$stops
  if(!is.null(syn.network)) {
    trips <- gtfs.obj$trips %>% dplyr::select(c("trip_id", "route_id"))
    stop.times <- base::merge(stop.times, trips, by = "trip_id")
    stop.times <- stop.times[stop.times$route_id %in% syn.network,]
  }
  stops.frequency <- as.data.frame(dplyr::count(stop.times, stop_id, sort = TRUE, name = "occ"))[c(1:num.stops),]
  stops.frequency$label <- NA
  for(i in 1:nrow(stops.frequency)) {
    stops.frequency$label[i] <- stops$stop_name[stops$stop_id == stops.frequency$stop_id[i]]
  }
  stops.frequency$lat <- 0
  stops.frequency$lon <- 0
  for(i in 1:nrow(stops.frequency)) {
    stops.frequency$lat[i] <- stops$stop_lat[stops$stop_id == stops.frequency$stop_id[i]]
    stops.frequency$lon[i] <- stops$stop_lon[stops$stop_id == stops.frequency$stop_id[i]]
  }

  stops.frequency$group <- NA
  stops.frequency$group[1] <- "1"
  let.tmp <- 1
  if(nrow(stops.frequency) > 1) {
    for(i in 2:nrow(stops.frequency)) {
      for(j in 1:(i-1)) {
        dist.check <- latlon_2_meter(stops.frequency$lat[i], stops.frequency$lon[i], stops.frequency$lat[j], stops.frequency$lon[j])
        if(dist.check <= 100) {
          if(is.na(stops.frequency$group[i])) {
            stops.frequency$group[i] <- stops.frequency$group[j]
          } else if(stops.frequency$group[j] %in% stops.frequency$group[i]) {

          } else {
            stops.frequency$group[i] <- paste(stops.frequency$group[i], stops.frequency$group[j], sep = ", ")
          }
        }
      }
      if(is.na(stops.frequency$group[i])) {
        let.tmp <- let.tmp + 1
        stops.frequency$group[i] <- as.character(let.tmp)
      }
    }
  }

  the.list <- list()
  for(i in 1:let.tmp) {
    ids.tmp <- numeric()
    for(j in 1:nrow(stops.frequency)) {
      if(as.character(i) %in% stops.frequency$group[j]) {
        ids.tmp <- c(ids.tmp, stops.frequency$stop_id[j])
      }
    }
    the.list[[i]] <- ids.tmp
  }

  names(the.list) <- as.character(seq(from = 1, to = let.tmp, by = 1))
  the.plot <- ggplot2::ggplot(data = stops.frequency, ggplot2::aes(x = label, y = occ)) +
    ggplot2::geom_bar(stat = "identity", width = 0.7 ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if(!("shape_id" %in% names(gtfs.obj$trips)) | !("shapes" %in% names(gtfs.obj))) {
    stop("Mapping could not be completed due to missing shape information.")
  } else {
    ids.to.map <- NULL
    for(i in 1:length(the.list)){
      ids.to.map <- c(ids.to.map, as.character(the.list[[i]][1]))
    }
    lats.to.map <- stops.frequency$lat[stops.frequency$stop_id %in% ids.to.map]
    lons.to.map <- stops.frequency$lon[stops.frequency$stop_id %in% ids.to.map]
    the.map <- gtfs_mapper(gtfs.obj, ag.name = NULL, route.ids = syn.network, opacity = 0.7, verbose = FALSE, tileset = NULL, include.legend = FALSE)
    blueIcon <- leaflet::makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
      iconWidth = 25,
      iconHeight = 41,
      iconAnchorX = 12,
      iconAnchorY = 41,
      shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
      shadowHeight = 41,
      shadowWidth = 41
    )
    the.map <- the.map %>% leaflet::addMarkers(lng = lons.to.map, lat = lats.to.map, icon = blueIcon, label = names(the.list))
  }
  return.list <- list(stops_plot = the.plot, stops_table = stops.frequency, map = the.map, stops_list = the.list)
  return(return.list)
}
