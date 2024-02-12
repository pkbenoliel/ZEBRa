addNewRow <- function(data) {
  for(i in 1:ncol(data)) {
    new.cell <- switch(class(data[1,i]),
                       "numeric" = 0,
                       "character" = "Placeholder",
                       NULL
    )
    if(i == 1) {
      new.row <- new.cell
    } else {
      new.row <- c(new.row, new.cell)
    }
  }
  data <- rbind(data, new.row)
  return(data)
}

deleteRow <- function(data, i) {
  return(data[-i,])
}

calculateCost <- function(bus.table, opp.chargers, energy.table) {
  # TOTAL COST BREAKDOWN
  # CAPITAL COSTS
    # Vehicles
    # Infrastructure
  # X YEARS COST
    # Energy
    # Maintenance costs
}

energyCosts <- function(gtfsObj, opYears, energyMode, routesMode, busTable, energyTable = NULL, routesTable = NULL) {
  if(!(energyMode %in% c(1, 2))) {
    stop("Energy table mode not selected, or an unidentified mode was provided.")
  }
  if(!(routesMode %in% c(1, 2))) {
    stop("Route table mode not selected, or an unidentified mode was provided.")
  }
  energyErrorMessage <- "The energy table was not found, or did not match the expected table format. Please double-check mode and format."
  routesErrorMessage <- "The routes table was not found, or did not match the expected table format. Please double-check mode and format."
  if(is.null(energyTable)) {
    stop(energyErrorMessage)
  }
  switch(energyMode,
         {
           #Mode 1: generated using Maps API (either in-situ or uploaded)
           switch(routesMode,
                  {
                    # Mode 1: Read from GTFS
                    listRoutes <- gtfsObj$routes$route_id
                    for(route in listRoutes) {
                      theseTrips <- gtfsObj$trips[gtfsObj$trips$route_id == route,]
                      #Highest daily trips on heaviest service day
                      numberTrips <- max((theseTrips %>% group_by(service_id) %>% summarize(n = n()))$n)
                      theseEnergies <- energyTable[energyTable$route_short_name == gtfsObj$routes$route_short_name[gtfsObj$routes$route_id == route],]
                      energyDemand <- numberTrips*theseEnergies
                    }
                  },
                  {
                    # Mode 2: Simplified route table
                  }
                  )
         },
         {
           #Mode 2: simplified energy method

         }
         )
}
