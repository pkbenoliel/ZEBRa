% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energyTabler.R
\name{gtfs_lengthr}
\alias{gtfs_lengthr}
\title{GTFS Lengther}
\usage{
gtfs_lengthr(
  gtfs.obj,
  skip = 0,
  batch.size = 50,
  location = "./GTFS Local Data/GMapsResults/",
  logs = FALSE,
  quiet = TRUE,
  ignore.local = FALSE
)
}
\arguments{
\item{gtfs.obj}{a GTFS object generated by the function \code{tidytransit::read_gtfs()}}

\item{skip}{An integer describing the number of rows at the start of the gtfs object to skip.}

\item{batch.size}{An integer describing how many rows to work on at once. This is primarily used for two reasons: first, it may be necessary to limit the number of simultaneous (or near simultaneous) calls made to the Google Maps API; second, it can be useful for error tracking. The default value of 50 can generally be used without issue.}

\item{location}{A character string describing which file to save the results to. Defaults to "./GTFS Local Results/GMapsResults".  The directory must exist.}

\item{logs}{Logical. If \code{TRUE}, the system will keep and store a log for troubleshooting}

\item{quiet}{Logical. If \code{FALSE}, the system will print messages to the terminal when it starts each batch and when the last batch is complete.}

\item{ignore.local}{Logical. If \code{FALSE}, the save location will be checked to see if a local file already exists. If it does, the local file will be used instead of generating a new \code{gmapsresults}.}
}
\value{
A new gtfs.obj with the lengths and times included in the stop_times dataframe.
}
\description{
This function adds missing time and length information about the routes using the gmapsdistance package.
}
\examples{
gtfs_length(gtfs.obj)

}
