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
