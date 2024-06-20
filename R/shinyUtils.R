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

calculateCost <- function(gtfsObj, operatingYears, energyCostKwh, energyModeLabel, timetableModeLabel, busTable, energyTable = NULL, routesTable = NULL, oppChargers, oppRate, oppCost, oppDuty, depotRate, depotCost) {
  # TOTAL COST BREAKDOWN
  # CAPITAL COSTS
    # Vehicles
    # Infrastructure
  # X YEARS COST
    # Energy
    # Maintenance costs
  #-------------------------------------
  timetableMode <- switch(timetableModeLabel,
                          gtfs = 1,
                          manual = 2
  )
  costsList <- vector(mode = "list", length = nrow(busTable))
  # First, we do everything per bus type
  for(i in 1:nrow(busTable)) {
    #Energy costs
    busVector <- NULL
    for(j in 1:ncol(busTable)) {
      busVector <- c(busVector, busTable[i, j])
    }
    energyCostDay <- energyCosts(gtfsObj, energyCostKwh, energyModeLabel, timetableModeLabel, busVector, energyTable, routesTable)
    operatingDays <- 360 #days per year. this value allows for holidays, weather inclement, other days of 'non-service'. In the future, we may allow this value to be changed.
    energyCostTotal <- energyCostDay*operatingDays*operatingYears

    #Infrastructure costs
    #----------------------
    # PROCEDURE
    # 1: Infer energy demand from cost (since cost is calculated from demand anyway)
    # 2: Calculate how much energy is provided by on route charging
    # 3: Calculate 'downtime' and energy deficit
    # 4: Purchase depot chargers to meet the deficit

    energyDemand <- energyCostDay/energyCostKwh
    switch(timetableMode, {
      # Mode 1: read from GTFS
      recordedTimes <- gtfsObj$stop_times[complete.cases(gtfsObj$stop_times),]
      operatingHours <- (max(hms_to_time(recordedTimes$arrival_time)) - min(hms_to_time(recordedTimes$arrival_time)))/60
    },
    {
      # Mode 2: simplified timetable
      operatingHours <- 0
      for(j in 1:nrow(routesTable)) {
        thisRouteHours <- (routesTable$HeadwayMinutes[i]*(routesTable$NumberTrips[i]-1) + routesTable$TripMinutes[i])/60
        if(thisRouteHours > operatingHours) {
          operatingHours <- thisRouteHours
        }
      }
    }
    )
    if(operatingHours > 24) {
      operatingHours <- 24
    }
    oppChargingHours <- oppDuty*operatingHours
    #We allow for an "extra 8 hours" per day since it is unlikely that buses will be operating 'round the clock' unless 100% of their energy is provided by opportunity charging
    downTime <- 32 - operatingHours
    oppEnergy <- oppChargingHours * oppRate * oppChargers
    energyDeficit <- energyDemand - oppEnergy
    if(energyDeficit < 0) {
      energyDeficit <- 0
    }
    depotChargers <- ceiling(energyDeficit/(depotRate*downTime))
    infrastructureCost <- oppChargers*oppCost*1000 + depotChargers*depotCost*1000

    #Vehicle Costs
    switch(timetableMode, {
      #We get 2 numbers for required buses, 1 from routes, and one from energy
      #For routing, we need the timetable. 1 bus per route, plus another bus if there are overlapping trips
      #Mode 1: read from GTFS
      busesForTimetable <- 0
      serviceIDTable <- gtfsObj$trips %>% group_by(service_id) %>% summarize(n = n())
      serviceID <- serviceIDTable$service_id[serviceIDTable$n == max(serviceIDTable$n)]
      if(length(serviceID > 1)) {
        serviceID <- serviceID[1]
      }
      tripsInID <- gtfsObj$trips %>% filter(service_id == serviceID) %>%
        left_join(gtfsObj$stop_times, by = "trip_id", relationship = "one-to-many")
      routes <- unique(tripsInID$route_id)
      for(route in routes) {
        theseTrips <- filter(tripsInID, route_id == route)
        tripStartEnd <- data.frame("trip_id" = NULL, "arrival_time" = NULL, "stop_sequence" = NULL)
        for(trip in unique(theseTrips$trip_id)) {
          thisTrip <- theseTrips %>% filter(trip_id == trip)
          thisTrip$arrival_time <- hms_to_time(thisTrip$arrival_time)
          thisTimes <- thisTrip %>% filter(stop_sequence %in% c(1, max(thisTrip$stop_sequence))) %>% select(trip_id, arrival_time, stop_sequence)
          tripStartEnd <- rbind(tripStartEnd, thisTimes)
        }
        tripStartEnd <- arrange(tripStartEnd, by = arrival_time)
        busesOut <- 0
        busesMax <- 0
        for(j in 1:nrow(tripStartEnd)) {
           if(tripStartEnd$stop_sequence[j] == 1) {
             busesOut <- busesOut + 1
             if(busesOut > busesMax) {
               busesMax <- busesOut
             }
           } else {
             busesOut <- busesOut - 1
           }
        }
        busesForTimetable <- busesForTimetable + busesMax
      }
    },
    {
      #Mode 2: simplified timetable method - simply length divided by frequency (simplified method assumes trips are spread evenly throughout the day)
      busesForTimetable <- 0
      for(index in 1:nrow(routesTable)) {
        busesForTimetable <- busesForTimetable + ceiling(routesTable$TripMinutes[index]/routesTable$HeadwayMinutes[index])
      }
    })
    #For energy cost, there have to be enough batteries to cover the energy deficit + 30% (chosen arbitrarily)
    busesForEnergy <- ceiling((energyDeficit*1.3)/as.numeric(busTable$PackCapacity[i]))
    vehicleCost <- max(busesForEnergy, busesForTimetable)*as.numeric(busTable$Cost[i])
    #Maintenance Costs are just pulled from table directly
    maintenanceCost <- as.numeric(busTable$AnnualizedMaintenanceCost[i]) * operatingYears

    listEntry <- list("MaintenanceCost" = round(maintenanceCost, digits = -3), "EnergyCost" = round(energyCostTotal, digits = -3),
                      "Infrastructure" = round(infrastructureCost, digits = -3), "Vehicles" = max(busesForEnergy, busesForTimetable),
                      "VehicleCost" = round(vehicleCost, digits = -3), "DepotChargers" = depotChargers)
    costsList[[i]] <- listEntry
  }
  names(costsList) <- busTable$Label
  return(costsList)
}

energyCosts <- function(gtfsObj, energyCostKwh, energyModeLabel, timetableModeLabel, busVector, energyTable = NULL, routesTable = NULL) {
  energyMode <- switch(energyModeLabel,
         upload = 1,
         generate = 1,
         simple = 2)
  if(is.null(energyMode)) {
    stop("Energy table mode not selected, or an unidentified mode was provided.")
  }
  timetableMode <- switch(timetableModeLabel,
                          gtfs = 1,
                          manual = 2)
  if(!(timetableMode %in% c(1, 2))) {
    stop("Route table mode not selected, or an unidentified mode was provided.")
  }
  energyErrorMessage <- "The energy table was not found, or did not match the expected table format. Please double-check mode and format."
  routesErrorMessage <- "The routes table was not found, or did not match the expected table format. Please double-check mode and format."
  if(is.null(energyTable)) {
    stop(energyErrorMessage)
  }
  busLabel <- busVector[1]
  busCost <- busVector[2]
  busMass <- busVector[3]
  busCapacity <- busVector[4]
  switch(energyMode,
         {
           #Mode 1: generated using Maps API (either in-situ or uploaded)

           switch(timetableMode,
                  {
                    # Mode 1: Read from GTFS
                    listRoutes <- gtfsObj$routes$route_id
                    energyDemand <- 0
                    for(route in listRoutes) {
                      theseTrips <- gtfsObj$trips[gtfsObj$trips$route_id == route,]
                      #Highest daily trips on heaviest service day
                      numberTrips <- max((theseTrips %>% group_by(service_id) %>% summarize(n = n()))$n)
                      routeEnergy <- energyTable$Electric_kWh[energyTable$route_short_name == gtfsObj$routes$route_short_name[gtfsObj$routes$route_id == route] & energyTable$bus_type == busLabel]*
                        energyTable$miles[energyTable$route_short_name == gtfsObj$routes$route_short_name[gtfsObj$routes$route_id == route] & energyTable$bus_type == busLabel]
                      energyDemand <- energyDemand + (numberTrips*routeEnergy)
                    }
                    energyCost <- energyDemand*energyCostKwh
                  },
                  {
                    # Mode 2: Simplified timetable
                    if(is.null(routesTable)) {
                      stop(routesErrorMessage)
                    }
                    energyDemand <- 0
                    for(route in routesTable$Route) {
                      numberTrips <- routesTable$NumberTrips[routesTable$Route == route]
                      routeEnergy <- energyTable$Electric_kWh[energyTable$route_short_name == gtfsObj$routes$route_short_name[gtfsObj$routes$route_short_name == route] & energyTable$bus_type == busLabel]
                      energyDemand <- energyDemand + (numberTrips*routeEnergy*routesTable$TripMiles[routesTable$Route == route])
                    }
                    energyCost <- energyDemand*energyCostKwh
                  }
                  )
         },
         {
           #Mode 2: simplified energy method
           switch(timetableMode,
                  {
                    # Mode 1: Read from GTFS
                    listRoutes <- gtfsObj$routes$route_id
                    energyDemand <- 0
                    for(route in listRoutes) {
                      theseTrips <- gtfsObj$trips[gtfsObj$trips$route_id == route,]
                      #Highest daily trips on heaviest service day
                      numberTrips <- max((theseTrips %>% group_by(service_id) %>% summarize(n = n()))$n)
                      routeEnergy <- energyTable$kWhPerMile[energyTable$Bus == busLabel]
                      energyDemand <- energyDemand + (numberTrips*routeEnergy)
                    }
                    energyCost <- energyDemand*energyCostKwh
                  },
                  {
                    # Mode 2: Simplified route table
                    if(is.null(routesTable)) {
                      stop(routesErrorMessage)
                    }
                    energyDemand <- 0
                    for(route in routesTable$Route) {
                      numberTrips <- routesTable$NumberTrips[routesTable$Route == route]
                      routeEnergy <- energyTable$kWhPerMile[energyTable$Bus == busLabel]
                      energyDemand <- energyDemand + (numberTrips*routeEnergy*routesTable$TripMiles[routesTable$Route == route])
                    }
                    energyCost <- energyDemand*energyCostKwh
                  }
                  )
         }
         )

  return(energyCost)
}

renderDollars <- function(costValue) {
  return(paste("$", formatC(costValue, format = "f", digits = 2, big.mark = ","), sep = ""))
}

calculateDiesel <- function(energyDemand, dieselCost) {
  effLow <- 0.25
  effHigh <- 0.4
  kWhPerGallon <- 38.09 #for diesel
  lowValue <- energyDemand/(kWhPerGallon * effLow)
  highValue <- energyDemand/(kWhPerGallon * effHigh)
  return(c(highValue, lowValue))
}
