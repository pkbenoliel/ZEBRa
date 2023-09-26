#Libraries
library(shiny)
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
  titlePanel("ZEBRa-Lite"),
  tabsetPanel(
    tabPanel("Dashboard",
             column(4,
                    fileInput("gtfsFile", "GTFS Feed", buttonLabel = "Upload Feed"),
                    actionButton("goButton", "Calculate"),
                    radioButtons("candLocSpec", "What do you want to base candidate location fitness off of?",
                                 c("Serving the most vehicles and routes" = "mode1",
                                   "Serving the highest mileage routes" = "mode2",
                                   "Serving the most frequently run routes" = "mode3")),
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
    tabPanel("Infrastructure Parameters",
             numericInput("oppCost", "Opportunity charger cost (thousand $USD):", value = 200, min = 0),
             numericInput("depCost", "Depot charger cost (thousand $USD):", value = 60, min = 0),
             numericInput("oppRate", "Rate of recharge for opportunity chargers (kW):", value = 350, min = 0),
             numericInput("depRate", "Rate of recharge for opportunity chargers (kW):", value = 90, min = 0),
             numericInput("energyCost", "Cost of energy ($USD per kWh):", value = 0.12, min = 0)
    ),
    tabPanel("Energy Table",
             radioButtons("energySource", "How do you want to generate the energy table?",
                          c("Upload an existing file" = "upload",
                            "Generate a table using the google maps API" = "generate",
                            "Use the simplified energy use method" = "simple")),
             uiOutput("energyDyn"),
             uiOutput("energyDyn2")
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
      actionButton("energyGo", "Calculate")
    } else {
      DTOutput("energySimplified")
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

  observeEvent(input$energySimplified_cell_edit, {
    info <- input$energySimplified_cell_edit
    i <- as.numeric(info$row)
    j <- as.numeric(info$col)+1
    k <- as.numeric(info$value)
    virtEnergyChoices[i,j] <- k
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
      virtEnergyChoices[i,1] = k
    } else {
      k = as.numeric(info$value)
    }
    virtBusTable$data[i,j] <- k
  })

  observeEvent(input$delRowBut, {
    req(nrow(virtBusTable$data) > 1)
    virtBusTable$data <- deleteRow(virtBusTable$data, input$delRowSel)
  })

  observeEvent(input$addRow, {
    virtBusTable$data <- addNewRow(virtBusTable$data)
  })

  output$energySimplified <- renderDT({
    DT::datatable(virtEnergyChoices$data, editable = list(target = "row", disable = list(columns = 0)), rownames = FALSE)
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

  observeEvent(input$goButton, {
    req(input$gtfsFile)
    gtfsObj <- tidytransit::read_gtfs(input$gtfsFile$datapath)
    cand.locs.tmp <- cand_loc_searcher(gtfsObj, verbose = FALSE, include.legend = FALSE, include.depots = FALSE)
    cand.map$data <- cand.locs.tmp$map
    cand.table$data <- cand.locs.tmp$stops_table
    showNotification("Complete!")
  })

  output$outputMap <- renderLeaflet({
    cand.map$data
  })

  output$stopsList <- renderTable({
    req(!is.null(cand.table$data))
    tibble(cand.table$data) %>%
                     select(stop_id, occ, label, group) %>%
                     rename("StopID" = stop_id, "TimesStopped" = occ, "StopLabel" = label, "ClusterNumber" = group)
  })

  output$testText <- renderText({
    paste0("Energy on test is ", virtEnergyChoices$data$kWhPerMile[1])
  })
}

shinyApp(ui, server)
