% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mappers.R
\name{gtfs_mapper}
\alias{gtfs_mapper}
\title{New GTFS Mapper}
\usage{
gtfs_mapper(
  gtfs.obj,
  ag.name = NULL,
  route.ids = NULL,
  service.ids = NULL,
  shape.ids = NULL,
  opacity = 0.7,
  route.colors = NULL,
  verbose = FALSE,
  tileset = NULL,
  include.legend = TRUE
)
}
\arguments{
\item{gtfs.obj}{a GTFS object created by \code{gtfs_loader()}.}

\item{ag.name}{a string containing the name to be used in the legend. Defaults to \code{NULL}. If \code{NULL}, information will be drawn from the \code{agency} of \code{gtfs.obj}.}

\item{route.ids}{a vector of route ids to be mapped. Defaults to \code{NULL}. If \code{NULL}, all route ids will be used.}

\item{service.ids}{a vector of service ids to be mapped. See \code{??gtfsr::get_routes_sldf} for more information.}

\item{shape.ids}{a vector of shape ids to be mapped. See \code{??gtfsr::get_routes_sldf} for more information.}

\item{opacity}{a number between 0 and 1 to serve as the opacity value for routes on the map. Defaults to \code{0.7}.}

\item{route.colors}{a vector of colors to be used on the route. Defaults to \code{NULL}. If \code{NULL}, a default set of colors will be used. See \code{??gtfsr::get_routes_sldf} for more information.}

\item{verbose}{a boolean. If \code{TRUE}, will display status messages.}

\item{tileset}{a character naming the \code{leaflet} tileset to use. See \code{names(providers)} with leaflet attached for a list of values. If \code{NULL}, the default leaflet tileset will be used.}

\item{include.legend}{a boolean. If \code{TRUE}, a legend for routes will be displayed on the map output.}
}
\value{
A leaflet map object.
}
\description{
This function replaces the \code{map_gtfs()} with supported functionality in \code{leaflet}.
}
