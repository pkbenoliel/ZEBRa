#Libraries
library(shiny)
library(shinyjs)
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


## ----------------------|
## UI CODE               |
## ----------------------|

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("ZEBRa-Lite"),
  tabsetPanel(
    tabPanel("Dashboard",
             column(6,
                    fileInput("gtfsFile", "GTFS Feed", buttonLabel = "Upload Feed"),
                    numericInput("oppChargers", "How many opportunity chargers do you want to use?", value = 0, min = 0),
                    numericInput("oppCost", "Opportunity charger cost (thousand $USD):", value = 200, min = 0),
                    numericInput("depCost", "Depot charger cost (thousand $USD):", value = 60, min = 0),
                    numericInput("oppRate", "Rate of recharge for opportunity chargers (kW):", value = 350, min = 0),
                    numericInput("depRate", "Rate of recharge for opportunity chargers (kW):", value = 90, min = 0),
                    numericInput("energyCost", "Cost of energy ($USD per kWh):", value = 0.12, min = 0),
                    actionButton("goButton", "Calculate")
                    ),
             column(6,

                    radioButtons("candLocSpec", "What do you want to base candidate location fitness off of?",
                                 c("Providing a charger to each route" = "mode1",
                                   "Create the most charging opportunities" = "mode2",
                                   "Serving the highest mileage routes" = "mode3",
                                   "Serving the most frequently run routes" = "mode4"))
                    )
    ),
    tabPanel("Results",
             column(4,
                    textOutput("testText")
                    ),
             column(8,
                    leafletOutput("outputMap")
                    )
    ),
    tabPanel("Details",
             column(4
                    ),
             column(8,
                    tableOutput("stopsList")
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
               "PackCapacity" = 1)
  })

  virtEnergyChoices <- reactiveValues(data = {
    data.frame("Bus" = "Placeholder",
               "kWhPerMile" = 0)
  })

  virtTimeTable <- reactiveValues(data = {
    data.frame("Route" = character(),
               "HeadwayMins" = numeric(),
               "NumberTrips" = numeric(),
               "TripLength" = numeric())
  })

  candLocMode <- reactiveVal(0)

  observeEvent(input$candLocSpec, {
    if(input$candLocSpec == "mode1") {
      shinyjs::disable("oppChargers")
      candLocMode(1)
    } else if(input$candLocSpec == "mode2") {
      shinyjs::enable("oppChargers")
      candLocMode(2)
    } else if(input$candLocSpec == "mode3") {
      shinyjs::enable("oppChargers")
      candLocMode(3)
    } else if(input$candLocSpec == "mode4") {
      shinyjs::enable("oppChargers")
      candLocMode(4)
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

  observeEvent(input$readRoutes, {
    req(input$gtfsFile)
    gtfsObj <- tidytransit::read_gtfs(input$gtfsFile$datapath)
    routeNames <- gtfsObj$routes$route_short_name
    virtTimeTable$data <- virtTimeTable$data[0,]
    for(i in 1:length(routeNames)) {
      tempFrame <- data.frame("Route" = routeNames[i], "HeadwayMins" = 1, "NumberTrips" = 1, "TripLength" = 1)
      virtTimeTable$data <- rbind(virtTimeTable$data, tempFrame)
    }
  })

  observeEvent(input$goButton, {
    req(input$gtfsFile)
    gtfsObj <- tidytransit::read_gtfs(input$gtfsFile$datapath)
    cand.locs.tmp <- cand_loc_searcher(gtfsObj, verbose = FALSE, include.legend = FALSE, include.depots = FALSE)
    cand.map$data <- cand.locs.tmp$map
    cand.table$data <- cand.locs.tmp$stops_table
    output$testText <- renderText({"A Calculate command was detected."})
    showNotification("Complete!")
  })

  output$outputMap <- renderLeaflet({
    cand.map$data
  })

  output$timeTable <- renderDT({
    DT::datatable(virtTimeTable$data, editable = list(target = "cell", disable = list(columns = 0)), rownames = FALSE)
  })

  output$stopsList <- renderTable({
    req(!is.null(cand.table$data))
    tibble(cand.table$data) %>%
                     select(stop_id, occ, label, group) %>%
                     rename("StopID" = stop_id, "TimesStopped" = occ, "StopLabel" = label, "ClusterNumber" = group)
  })

}

shinyApp(ui, server)


