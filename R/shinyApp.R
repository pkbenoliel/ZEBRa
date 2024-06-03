#Libraries
library(shiny)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(tidytransit)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(DT)
library(reshape2)
library(magrittr)
library(sf)
library(markdown)
library(knitr)


## ----------------------|
## UI CODE               |
## ----------------------|

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("ZEBRa-Lite"),
  tabsetPanel(
    tabPanel("Instructions",
             uiOutput("instructions")
             ),
    tabPanel("Dashboard",
             column(6,
                    actionButton("exampleButton", "Load Example Data"),
                    textOutput("exampleDataText"),
                    fileInput("gtfsFile", "GTFS Feed", buttonLabel = "Upload Feed"),
                    numericInput("oppChargers", "How many opportunity chargers do you want to use?", value = 0, min = 0),
                    numericInput("operatingYears", "Number of years the bus is planned to operate:", value = 12, min = 1),
                    numericInput("energyCost", "Cost of energy ($USD per kWh):", value = 0.12, min = 0),
                    numericInput("diesel", "Cost per gallon of diesel ($USD) (optional):", value = 0, min = 0),
                    bsTooltip("energyCost", "A flat energy cost per kWh applied to the whole system. This value is used to calculate expected running costs.", "right", options = list(container = "body")),
                    actionButton("mapButton", "Draw Map"),
                    actionButton("goButton", "Calculate"),
                    bsTooltip("gtfsFile", "Click here to load a GTFS file from your computer. Upload a .zip file containing all necessary GTFS data.", "left", options = list(container = "body")),
                    bsTooltip("exampleButton", "Click here to load example data.", "right", options = list(container = "body")),
                    bsTooltip("oppChargers", "Enter the number of chargers to be used.  Only active if the mode of 'Provide a charger to each route' is not selected.", "right", options = list(container = "body"))
                    ),
             column(6,

                    radioButtons("candLocSpec", "What do you want to base candidate location fitness off of?",
                                 c("Providing a charger to each route" = "mode1",
                                   "Create the most charging opportunities (most frequent stops)" = "mode2"
#                                   "Serving the highest mileage routes" = "mode3",
#                                   "Serving the most frequently run routes" = "mode4"
                                   )),
                    textOutput("candLocWarn"),
                    tags$head(tags$style("#candLocWarn{color: red; font-style: italic")),
                    numericInput("dutyRatio", "Select expected duty ratio of the opportunity chargers (%):", min = 10, max = 90, value = 25),
                    numericInput("oppCost", "Opportunity charger cost (thousand $USD):", value = 200, min = 0),
                    numericInput("depCost", "Depot charger cost (thousand $USD):", value = 60, min = 0),
                    numericInput("oppRate", "Rate of recharge for opportunity chargers (kW):", value = 350, min = 0),
                    numericInput("depRate", "Rate of recharge for depot chargers (kW):", value = 90, min = 0)
                    )
    ),
    tabPanel("Results",
             column(4,
                    textOutput("totalCostText"),
                    textOutput("capitalCostText"),
                    textOutput("dieselCostText"),
                    plotOutput("costPie")
                    ),
             column(8,
                    tabsetPanel(
                      tabPanel("Map",
                               leafletOutput("outputMap")
                               ),
                      tabPanel("List of Stops",
                               tableOutput("stopsList")
                               ),
                      tabPanel("Results Details",
                               tableOutput("outputTable")
                               )
                      )
                    )
    ),
    tabPanel("Bus Parameters",
             sidebarLayout(
               sidebarPanel(
                 numericInput("delRowSel","Select row to delete:", value = 1, min = 1),
                 actionButton("delRowBut", "Delete", class = "btn btn-danger"),
                 actionButton("addRow", "Add Row", class = "btn btn-success")
               ),
               mainPanel(
                 DTOutput("busTable")
               )
            )
    ),
    tabPanel("Energy Table",
             radioButtons("energySource", "How do you want to generate the energy table?",
                          c("Upload an existing file" = "upload",
                            "Generate a table using the google maps API" = "generate",
                            "Use the simplified energy use method" = "simple")),
             uiOutput("energyDyn"),
             uiOutput("energyDyn2")
    ),
    tabPanel("Timetable Information",
             radioButtons("timeTableSrc", "How do you want to read the route time table?",
                          c("Read from GTFS (Recommended, only if the GTFS feed is validated)" = "gtfs",
                            "Input values manually" = "manual")),
             uiOutput("routeButtonPlace"),
             uiOutput("timeTablePlace")
    ),
    tabPanel("Candidate Locations",
             HTML("<I>Functionality will be added in the future to manually include or exclude stops from being candidates for opportunity charging. Stay tuned!</I>")
    )
  )
)

## ----------------------|
## SERVER CODE           |
## ----------------------|

server <- function(input, output, session) {
  gtfs.obj <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           zip = tidytransit::read_gtfs(input$upload$datapath),
           validate("Invalid filetype; please upload a zip file.")
    )

  })

  output$energyDyn <- renderUI({
    if(input$energySource == "upload") {
      fileInput("energyTable", "Upload Energy Table", accept = ".csv")
    } else if(input$energySource == "generate") {
      textInput("gmapsKey", "Google Maps API")
    } else {
      DTOutput("energySimplified")
    }
  })

  output$energyDyn2 <- renderUI({
    if(input$energySource == "generate") {
      actionButton("energyGo", "Calculate")
    } else {
      #Left empty on purpose
    }
  })

  output$routeButtonPlace <- renderUI({
    if(input$timeTableSrc == "manual") {
      actionButton("readRoutes", "Read Routes")
    } else {
      #Left empty on purpose
    }
  })

  output$timeTablePlace <- renderUI({
    if(input$timeTableSrc == "manual") {
      DTOutput("timeTable")
    } else {
      #Left empty on purpose
    }
  })

  virtBusTable <- reactiveValues(data = {
    data.frame("Label" = "Placeholder",
               "Cost" = 0,
               "Mass" = 1,
               "PackCapacity" = 1,
               "AnnualizedMaintenanceCost" = 1)
  })

  virtEnergyChoices <- reactiveValues(data = {
    data.frame("Bus" = "Placeholder",
               "kWhPerMile" = 0)
  })

  virtTimeTable <- reactiveValues(data = {
    data.frame("Route" = character(),
               "HeadwayMinutes" = numeric(),
               "NumberTrips" = numeric(),
               "TripMiles" = numeric(),
               "TripMinutes" = numeric())
  })

  output$instructions <- renderUI({
    HTML(markdown::markdownToHTML(knit("ZEBRaLite/INSTRUCTION_TAB.Rmd", quiet = TRUE)))
  })

  candLocMode <- reactiveVal(0)
  energyMode <- reactiveVal(0)
  routesMode <- reactiveVal(0)
  exampleData <- reactiveVal(FALSE)

  observeEvent(input$gtfsFile, {
    exampleData(FALSE)
  })

  observeEvent(input$candLocSpec, {
    if(input$candLocSpec == "mode1") {
      shinyjs::disable("oppChargers")
      candLocMode(1)
      output$candLocWarn <- renderText(NULL)
    } else if(input$candLocSpec == "mode2") {
      shinyjs::enable("oppChargers")
      candLocMode(2)
      output$candLocWarn <- renderText(NULL)
    } else if(input$candLocSpec == "mode3") {
      shinyjs::enable("oppChargers")
      candLocMode(3)
      output$candLocWarn <- renderText({"Requires accurate mileage data in an uploaded energy table, or entered via the \"Timetable Information\" tab. Please ensure that the data are entered correctly before using this mode."})
    } else if(input$candLocSpec == "mode4") {
      shinyjs::enable("oppChargers")
      candLocMode(4)
      output$candLocWarn <- renderText({"Requires accurate headway data in an uploaded energy table, or entered via the \"Timetable Information\" tab. Please ensure that the data are entered correctly before using this mode."})
    }
  })

  observeEvent(input$timeTable_cell_edit, {
    info <- input$timeTable_cell_edit
    i <- as.numeric(info$row)
    j <- as.numeric(info$col)+1
    k <- as.numeric(info$value)
    virtTimeTable$data[i,j] <- k
  })

  observeEvent(input$energySimplified_cell_edit, {
    info <- input$energySimplified_cell_edit
    i <- as.numeric(info$row)
    j <- as.numeric(info$col)+1
    k <- as.numeric(info$value)
    virtEnergyChoices$data[i,j] <- k
  })

  observeEvent(input$busTable_cell_edit, {
    info <- input$busTable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)+1
    if(j == 1) {
      k = as.character(info$value)
      if(nrow(virtBusTable$data) < nrow(virtEnergyChoices$data)) {
        virtEnergyChoices$data <- virtEnergyChoices$data(virtEnergyChoices$data$Bus %in% virtEnergyChoices$data$Label)
      }
      if(nrow(virtBusTable$data) > nrow(virtEnergyChoices$data)) {
        diff <- nrow(virtBusTable$data) - nrow(virtEnergyChoices$data)
        junk <- data.frame("Bus" = "", "kWhPerMile" = 0)
        for(c in 1:diff) {
          virtEnergyChoices$data <- rbind(virtEnergyChoices$data, junk)
        }
      }
      virtEnergyChoices$data[i,1] = k
    } else {
      k = as.numeric(info$value)
    }
    virtBusTable$data[i,j] <- k
  })

  observeEvent(input$delRowBut, {
    req(nrow(virtBusTable$data) > 1)
    virtBusTable$data <- deleteRow(virtBusTable$data, input$delRowSel)
    virtEnergyChoices$data <- deleteRow(virtEnergyChoices$data, input$delRowSel)
  })

  observeEvent(input$addRow, {
    virtBusTable$data <- addNewRow(virtBusTable$data)
    virtEnergyChoices$data <- addNewRow(virtEnergyChoices$data)
  })

  output$energySimplified <- renderDT({
    DT::datatable(virtEnergyChoices$data, editable = list(target = "cell", disable = list(columns = 0)), rownames = FALSE)
  })

  output$busTable <- renderDT({
    DT::datatable(virtBusTable$data, editable = TRUE, rownames = FALSE)
  })

  cand.map <- reactiveValues(data = {
    NULL
  })
  cand.table <- reactiveValues(data = {
    NULL
  })
  cost.pie <- reactiveValues(data = {
    NULL
  })
  output.table <- reactiveValues(data = {
    NULL
  })

  observeEvent(input$exampleButton, {
    exampleData(TRUE)
    gtfsObj <- tidytransit::read_gtfs("https://unitrans.ucdavis.edu/media/gtfs/Unitrans_GTFS.zip")
    #Example Buses
    exLabels <- c("35ft", "40ft", "60ft")
    exCost <- c(180000, 230000, 460000)
    exMass <- c(25000, 35000, 60000)
    exKWH <- c(120, 180, 300)
    exMaint <- c(5000, 5000, 5000)
    virtBusTable$data <- data.frame("Label" = exLabels, "Cost" = exCost, "Mass" = exMass, "PackCapacity" = exKWH, "AnnualizedMaintenanceCost" = exMaint)
    #Best guesses
    virtEnergyChoices$data <- data.frame("Bus" = exLabels, "kWhPerMile" = c(2.5, 4, 8))
    #Generate pseudo-random data for example simplified energy
    listRoutes <- gtfsObj$routes$route_short_name
    heads <- numeric()
    trips <- numeric()
    miles <- numeric()
    minutes <- numeric()
    for(route in listRoutes) {
      thisHead <- round(runif(1, min = 10, max = 75))
      #No more than 10 operating hours
      thisTrip <- round(runif(1, min = 5, max = 600/thisHead))
      thisMiles <- round(runif(1, min = 3, max = 24))
      #Average speed no slower than 10 mph, no faster than 45 mph
      thisMinutes <- round(runif(1, min = thisMiles*1.5, max = thisMiles*6))
      heads <- c(heads, thisHead)
      trips <- c(trips, thisTrip)
      miles <- c(miles, thisMiles)
      minutes <- c(minutes, thisMinutes)
    }
    virtTimeTable$data <- data.frame("Route" = listRoutes,
                                     "HeadwayMinutes" = heads,
                                     "NumberTrips" = trips,
                                     "TripMiles" = miles,
                                     "TripMinutes" = minutes)
  })

  observeEvent(input$readRoutes, {
    if(exampleData()) {
      #Static URL for Unitrans
      gtfsObj <- tidytransit::read_gtfs("https://unitrans.ucdavis.edu/media/gtfs/Unitrans_GTFS.zip")
    } else {
      req(input$gtfsFile)
      gtfsObj <- tidytransit::read_gtfs(input$gtfsFile$datapath)
    }
    routeNames <- gtfsObj$routes$route_short_name
    virtTimeTable$data <- virtTimeTable$data[0,]
    for(i in 1:length(routeNames)) {
      tempFrame <- data.frame("Route" = routeNames[i], "HeadwayMinutes" = 1, "NumberTrips" = 1, "TripMiles" = 1, "TripMinutes" = 1)
      virtTimeTable$data <- rbind(virtTimeTable$data, tempFrame)
    }
  })

  observeEvent(input$mapButton, {
    if(!exampleData()) {
      req(input$gtfsFile)
    }
    output.table$data <- NULL
    cost.pie$data <- NULL
    output$totalCostText <- renderText(NULL)
    output$capitalCostText <- renderText(NULL)
    showNotification("Mapping...", type = "message")
    if(!exampleData()) {
      gtfsObj <- tidytransit::read_gtfs(input$gtfsFile$datapath)
    } else {
      #Static URL for Unitrans
      gtfsObj <- tidytransit::read_gtfs("https://unitrans.ucdavis.edu/media/gtfs/Unitrans_GTFS.zip")
    }
    cand.locs.tmp <- cand_loc_mode(gtfsObj, candLocMode(), input$oppChargers) #syn.network defaults to NULL, we will add that functionality later.
    cand.map$data <- cand.locs.tmp$map
    if(is.character(cand.map$data)) {
      showNotification(cand.map$data, type = "error")
      cand.map$data <- NULL
    }
    cand.table$data <- cand.locs.tmp$stops_table
    showNotification("Complete!")
  })

  observeEvent(input$goButton, {
    if(!exampleData()) {
      req(input$gtfsFile)
    }
    req(cand.map$data)
    showNotification("Calculating...", type = "message")
    if(!exampleData()) {
      gtfsObj <- tidytransit::read_gtfs(input$gtfsFile$datapath)
    } else {
      #Static URL for Unitrans
      gtfsObj <- tidytransit::read_gtfs("https://unitrans.ucdavis.edu/media/gtfs/Unitrans_GTFS.zip")
    }
    if(exampleData()) {
      #static file location for energy data
      energyIn <- switch(input$energySource,
                         upload = read.csv("./Energy Tables/Unitrans Energy Table.csv"),
                         generate = read.csv("./Energy Tables/Unitrans Energy Table.csv"),
                         simple = virtEnergyChoices$data)
    } else {
      energyIn <- switch(input$energySource,
                         upload = read.csv(input$energyTable$datapath),
                         generate = read.csv(input$energyTable$datapath),
                         simple = virtEnergyChoices$data)
    }

    costsList <- suppressWarnings(calculateCost(gtfsObj = gtfsObj, operatingYears = input$operatingYears, energyCostKwh = input$energyCost, energyModeLabel = input$energySource,
                                                timetableModeLabel = input$timeTableSrc, busTable = virtBusTable$data, energyTable = energyIn, routesTable = virtTimeTable$data,
                                                oppChargers = nrow(cand.table$data), oppRate = input$oppRate, oppCost = input$oppCost, oppDuty = input$dutyRatio/100,
                                                depotRate = input$depRate, depotCost = input$depCost))
    minTotalCost <- Inf
    outputTableHolder <- rep(list(NULL), length(costsList))
    for(i in 1:length(costsList)) {
      thisBus <- names(costsList)[i]
      thisTotalCost <- sum(costsList[[i]]$MaintenanceCost, costsList[[i]]$EnergyCost, costsList[[i]]$Infrastructure, costsList[[i]]$VehicleCost)
      if(thisTotalCost < minTotalCost) {
        minTotalCost <- thisTotalCost
        minBusType <- thisBus
        minIndex <- i
      }
      varLabels <- c(names(costsList[[i]]), "OpportunityChargers")
      outputValues <- character()
      for(j in 1:length(costsList[[i]])) {
        if(j %in% c(4, 6)) {
          outputValues <- c(outputValues, as.character(costsList[[i]][[j]]))
        } else {
          outputValues <- c(outputValues, renderDollars(costsList[[i]][[j]]))
        }
      }
      outputValues <- c(outputValues, nrow(cand.table$data))
      thisDataFrame <- data.frame("Parameter" = varLabels, "Value" = outputValues)
      outputTableHolder[[i]] <- thisDataFrame

    }
    names(outputTableHolder) <- names(costsList)
    output.table$data <- outputTableHolder
    finalCost <- sum(costsList[[minIndex]]$Infrastructure, costsList[[minIndex]]$VehicleCost)
    output$totalCostText <- renderText({paste("The bus type with the lowest cost was ", minBusType, ", with a total system cost of ", renderDollars(minTotalCost),
                                              ".", sep = "")})
    output$capitalCostText <- renderText({paste("The capital costs (vehicles and infrastructure) are ", renderDollars(finalCost), ".", sep = "")})
    if(input$diesel != 0) {
      bestEnergy <- costsList[[minIndex]]$EnergyCost/input$energyCost
      dieselOut <- calculateDiesel(bestEnergy, diesel)
      output$dieselCostText <- renderText({paste("It is estimated that the savings in fuel cost over the lifetime of the buses is between ", renderDollars(dieselOut[1]), " and ", renderDollars(dieselOut[2]), ".",
        sep = "")})
    }
    valuesForPie <- numeric()
    for(i in 1:length(costsList)) {
      valuesForPie <- c(valuesForPie, costsList[[minIndex]][[i]])
    }
    dataForPie <- data.frame("Costs" = names(costsList[[minIndex]]), "Values" = valuesForPie)
    dataForPie <- dataForPie[dataForPie$Costs %in% c("MaintenanceCost", "EnergyCost", "Infrastructure", "VehicleCost"),]
    dataForPie$Costs <- c("Maintenance Cost", "Energy Cost", "Infrastructure Cost", "Vehicle Cost")
    cost.pie$data <- ggplot(data = dataForPie, aes(x = "", y = Values, fill = Costs)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("#022851", "#768493", "#D0BA79", "#FFBF00"))
      ggtitle("Pie Chart of Costs") +

      theme_void()
    showNotification("Complete!")
  })

  output$outputMap <- renderLeaflet({
    cand.map$data
  })

  output$costPie <- renderPlot({
    cost.pie$data
  })

  output$timeTable <- renderDT({
    DT::datatable(virtTimeTable$data, editable = list(target = "cell", disable = list(columns = 0)), rownames = FALSE)
  })

  output$stopsList <- renderTable({
    req(!is.null(cand.table$data))
    tibble(cand.table$data) %>%
                     select(stop_id, occ, label, group) %>%
                     rename("Stop ID" = stop_id, "Times Stopped" = occ, "Stop Label" = label, "Cluster Number" = group)
  })

  output$outputTable <- renderTable({
    output.table$data
  })

  output$exampleDataText <- renderText({
    if(exampleData()){
      stringToShow <- "Example data is LOADED."
    } else {
      stringToShow <- "Example data is UNLOADED"
    }
    stringToShow
  })

}

shinyApp(ui, server)


