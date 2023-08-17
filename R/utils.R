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
