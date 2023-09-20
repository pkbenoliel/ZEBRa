#' Rectify Column Names
#'
#' This function rectifies column names by removing erroneous byte order marks that can sometimes appear when importing GTFS objects.
#'
#' @param gtfs.obj an object created by TODO
#'
#' @return The GTFS obejct with rectified column names
#' @export
#'
#' @examples
#' rectify_colnames(gtfs.obj)
#' #Will return the GTFS object supplied, with any byte order makes (commonly appearing in text as "ï..").
#'
rectify_colnames <- function(gtfs.obj) {
  for(i in 1:length(gtfs.obj)) {
    df <- gtfs.obj[[i]]
    if(grepl(pattern = "ï..", names(df)[1])) {
      names(df)[1] <- substr(names(df)[1], start = 4, stop = nchar(names(df[1])))
    }
    gtfs.obj[[i]] <- df
  }
  return(gtfs.obj)
}

#' Convert time to dated time
#'
#' Converts the inputted time character to a POSIXct dated time. This is primarily used to handle schedules that run past midnight.
#'
#' @param time a character representing a time in the form %H:%M:%S. Hour values over 24 are acceptable.
#'
#' @return POSIXct
#' @export
#'
#' @examples
#' dater("14:35:43")
dater <- function(time) {
  if(is.na(time) | time == "") {
    time.out <- NA
  } else {
    tz <- "America/Los_Angeles"
    stop <- gregexpr(pattern = ":", text = time)[[1]][1]-1
    hour <- as.numeric(substr(time,1,stop))
    minsec <- substr(time,3,8)
    year <- lubridate::year(Sys.Date())
    if(hour >= 24){
      time.out <- as.POSIXct(x = paste(as.character(year), "/01/02 ", hour-24, minsec, sep = ""), format = "%Y/%m/%d %H:%M:%S", tz = tz)
    } else{
      time.out <- as.POSIXct(x = paste(as.character(year), "/01/01 ", time, sep = ""), format = "%Y/%m/%d %H:%M:%S", tz = tz)
    }
  }
  return(time.out)
}


#' Generate a date for January 1 of the current year
#'
#' @return Date, a date for January 1st of the current year
#' @export
#'
#' @examples
#'
jan_first <- function() {
  this.year <- lubridate::year(Sys.Date())
  date.char <- paste(as.character(this.year), "-01-01", sep = "")
  date.out <- as.Date(date.char)
  return(date.out)
}

#' What routes pass through this stop?
#'
#' @param gtfs.obj a network to analyze.
#' @param stop.id a stop id within the network to check on.
#'
#' @return a list of routes that pass through the stop.
#' @export
#'
#' @examples
#'
stop_on_routes <- function(gtfs.obj, stop.id) {
  stop.times <- gtfs.obj$stop_times
  trips <- gtfs.obj$trips
  list.of.trips <- unique(stop.times$trip_id[stop.times$stop_id == stop.id])
  list.of.routes <- unique(trips$route_id[trips$trip_id %in% list.of.trips])
  return(list.of.routes)
}

#' Find Distance between Two Lat/Long Pairs
#'
#' @param lat1 numeric, a latitude
#' @param lon1 numeric, a longitude
#' @param lat2 numeric, another latitude
#' @param lon2 numeric, another longitude
#'
#' @return Numeric, the distance in meters between the points.  Error is untested at this point.
#' @export
#'
#' @examples
#'
latlon_2_meter <- function(lat1, lon1, lat2, lon2) {
  dist.raw <- sqrt((lat1-lat2)^2 + (lon1-lon2)^2)
  output <- dist.raw*111139
  return(output)
}

#' Get shapes spatial data for given route ids
#' This function was originally part of the `gtfsr` package; the code was written by its authors:
#'
#' Elaine McVey (elaine.mcvey\@transloc.com)
#' Danton Noriega-Goodwin (danton.noriega\@gmail.com)
#'
#' gtfsr was distributed under the GPL license.  For more information, please see the GPL license website: https://www.gnu.org/licenses/gpl-3.0.en.html
#'
#'
#' @param gtfs.obj A GTFS list object with components agency, etc.
#' @param route_ids Vector (Character). IDs for routes of interest.
#' @param service_ids Vector (Character). Service IDs. NULL by Default.
#' @param shape_ids Vector (Character). Shape IDs. NULL by Default.
#' @param route_opacity Numeric. Value must be between 0 and 1. Default is NULL.
#' @param route_colors Character. Names of colors (e.g. "blue") or hex values (e.g. '#000000').
#' @return Environment containing spatial data, labels, colorings used for plotting
#' @export
get_routes_sldf <- function(gtfs.obj, route_ids, service_ids, shape_ids, route_opacity, route_colors) {

  stopifnot("gtfs" %in% class(gtfs.obj),
            !is.null(gtfs.obj$shapes),
            !is.null(gtfs.obj$trips),
            !is.null(gtfs.obj$routes),
            length(route_ids) > 0)

  # check for bad route ids
  bad_route_ids <- route_ids[which(!route_ids %in% gtfs.obj$routes$route_id)]
  route_ids <- route_ids[which(route_ids %in% gtfs.obj$routes$route_id)]

  # error if all route ids are bad
  if(length(route_ids) == 0) {
    s <- "No provided Route ID(s) were found. Please provide valid Route IDs." %>% sprintf(paste(bad_route_ids, collapse = ", "))
    stop(s)
  }

  # warn if some route ids are omitted
  if(length(bad_route_ids) > 0) {
    s <- "Route ID(s) '%s' not found. Omitted." %>% sprintf(paste(bad_route_ids, collapse = ", "))
    warning(s)
  }

  if(!is.null(service_ids)) {

    # check service ids
    bad_service_ids <- service_ids[which(!service_ids %in% gtfs.obj$trips$service_id)]
    service_ids <- service_ids[which(service_ids %in% gtfs.obj$trips$service_id)]

    if(length(service_ids) == 0) {
      s <- "No provided Service ID(s) --- '%s' --- were found. Please provide valid Service IDs." %>% sprintf(paste(bad_service_ids, collapse = ", "))
      stop(s)
    }

    if(length(bad_service_ids) > 0) {
      s <- "Service ID(s) '%s' not found. Omitted." %>% sprintf(paste(bad_service_ids, collapse = ", "))
      warning(s)
    }

    shapes_routes <- gtfs.obj$trips %>%
      dplyr::slice(which(service_id %in% service_ids)) %>%
      dplyr::slice(which(route_id %in% route_ids)) %>%
      dplyr::select(shape_id, route_id, service_id) %>%
      dplyr::filter(!is.na(shape_id)) %>%
      dplyr::distinct(., service_id, shape_id, route_id, .keep_all = TRUE) # want only distinct routes

  } else {

    shapes_routes <- gtfs.obj$trips %>%
      dplyr::slice(which(route_id %in% route_ids)) %>%
      dplyr::select(shape_id, route_id, service_id) %>%
      dplyr::filter(!is.na(shape_id)) %>%
      dplyr::distinct(., service_id, shape_id, route_id, .keep_all = TRUE) # want only distinct routes

  }

  # extract or check for shape_ids
  if(is.null(shape_ids)) {
    shape_ids <- shapes_routes$shape_id
  } else {
    indx <- shape_ids %in% shapes_routes$shape_id
    if(length(shape_ids[indx]) == 0) {
      message("No user defined shape_ids found. Using all valid shape_ids.")
      shape_ids <- shapes_routes$shape_id
    } else {
      if(length(indx) > length(shape_ids[indx])) {
        s <- "Shape ID(s) '%s' not found. Omitted." %>% sprintf(paste(shape_ids[!indx], collapse = ", "))
        message(s)
      }

      # filter out user specified shapes and update
      shapes_routes %<>%
        dplyr::filter(shape_id %in% shape_ids[indx])
      shape_ids <- shapes_routes$shape_id

    }

  }

  # check if nothing is found
  if(length(shape_ids) == 0) {
    s <- "No shapes for Route ID '%s' were found." %>% sprintf(paste(route_ids, collapse = ", "))
    stop(s)
  }

  gtfs_trips <- gtfs.obj$trips %>%
    dplyr::slice(which(route_id %in% route_ids))

  # extract all shapes for given shape ids
  gtfs_shapes <- gtfs.obj$shapes %>%
    dplyr::slice(which(shape_id %in% shape_ids))

  # code was taken from `stplanr::gtfs2sldf` (package::function)
  sp_lines <- (gtfs_shapes %>% dplyr::rename(lat = shape_pt_lat, lon = shape_pt_lon) %>%
                 dplyr::group_by(shape_id) %>%
                 dplyr::arrange(shape_pt_sequence) %>% dplyr::do_(gtfsline = "sp::Lines(sp::Line(as.matrix(.[,c('lon','lat')])),unique(.$shape_id))") %>%
                 dplyr::ungroup() %>% dplyr::do_(gtfsline = "sp::SpatialLines(.[[2]], proj4string = sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))")) %>%
    magrittr::extract2('gtfsline') %>%
    magrittr::extract2(1)

  # get updated shape ids (order matters)
  shape_ids <- sapply(sp_lines@lines, function(x) x@ID)

  df <- shape_ids %>%
    as.data.frame %>%
    `rownames<-`(., shape_ids)

  gtfs_lines <- sp::SpatialLinesDataFrame(sp_lines, data = df)

  # OPACITY AND COLORS ------------------------------------------------
  ## route_colors
  if(!is.null(route_colors)) {
    if(length(route_colors) != length(route_ids)) {
      warning("route_colors and route_ids are not the same length. route_colors is ignored and default colors will be used.")
      route_colors <- scales::hue_pal()(length(route_ids))
    } else {
      route_colors <- scales::col2hcl(route_colors) %>%
        sapply(. %>% substr(.,1,7), USE.NAMES = FALSE)
    }
  } else {
    route_colors <- scales::hue_pal()(length(route_ids))
  }

  # extract corresponding route ids and names for shape ids
  routes_colors <- dplyr::data_frame(route_id = route_ids,
                                        color = route_colors) %>%
    dplyr::left_join(gtfs.obj$routes %>% dplyr::select(route_id, route_short_name), by = 'route_id')

  # merge colors to shape_routes
  shapes_routes_colors <- shapes_routes %>%
    dplyr::left_join(routes_colors, by = 'route_id')

  # popup maker
  gen_popups_routes <- function(a, b, c) {
    mapply(function(x, y, z) {
      text <- htmltools::tags$div(style = 'color:#000000',
                                  htmltools::tags$body(
                                    htmltools::span(htmltools::strong(paste('Route', x))), htmltools::br(),
                                    htmltools::span(htmltools::strong('Service ID: '), y), htmltools::br(),
                                    htmltools::span(htmltools::strong('Shape ID: '), z))
      )
      as.character(text)
    },
    x = a,
    y = b,
    z = c,
    SIMPLIFY = FALSE) %>%
      unlist %>%
      sapply(stringr::str_replace_all, '\n[ ]*', '') %>%
      stats::setNames(NULL)
  }

  # make color vector for shapes
  shapes_colors <- shapes_routes_colors %>%
    dplyr::slice(match(shape_ids, shape_id)) %>% # match helps resort rows so colors/labels match up with gtfs_lines (only works cause we have ONE of each shape)
    dplyr::group_by(route_id) %>%
    dplyr::mutate(n = n(), opacity = route_opacity/(n)) %>% # opacity is scaled by route numbers
    dplyr::mutate(labels = paste("Route", route_short_name)) %>%
    dplyr::mutate(popups = gen_popups_routes(route_id, service_id, shape_id)) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup() %>% # important to keep order correct!
    dplyr::select(shape_id, color, opacity, labels, popups) %>%
    dplyr::mutate(opacity = dplyr::if_else(opacity < 0.05, 0.05, opacity)) # opacity threshold

  # CHECKING ROUTES ------------------------------------------------
  # update/ensure route_ids carry correctly and sort correctly for plotting
  keep <- shapes_routes$route_id %>% unique()
  ids <- routes_colors$route_id

  indx <- match(ids, keep) %>% stats::na.omit()
  not_found <- ids[!ids %in% keep] #routes not found

  #check to see if routes were dropped
  if(length(ids) > length(indx)) {
    s <- sprintf("Shapes for route_id(s) %s are/were not found. Removed.", paste(not_found, collapse = ', '))
    message(s)
  }

  # update routes and keep only routes with shapes
  routes_colors %<>%
    dplyr::filter(route_id %in% keep)

  # return
  env <- new.env()
  lapply(c("gtfs_lines", "shapes_colors", "shapes_routes", "routes_colors"), function(x) assign(x, value = get(x), envir = env))
  return(env)

}
