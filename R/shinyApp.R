#Libraries
library(shiny)
library(leaflet)
library(tidytransit)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)

## ----------------------|
## UI CODE               |
## ----------------------|

ui <- fluidPage(
  titlePanel("ZEBRa-Lite"),
  tabsetPanel(
    tabPanel("Control Panel",
             fileInput("gtfsObj", "GTFS Feed", buttonLabel = "Upload Feed"),
    ),
    tabPanel("Results",
             sidebarLayout(
               sidebarPanel(

               ),
               mainPanel(
                 plotOutput("outputMap")
               )
             )
    ),
    tabPanel("Details"

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
    tabPanel("Charger Parameters"

    ),
    tabPanel("Energy Table",
             radioButtons("energySource", "How do you want to generate the energy table?",
                          c("Upload an existing file" = "upload",
                            "Generate a table using the google maps API" = "generate",
                            "Use the simplified energy use method" = "simple")),
             uiOutput("energyDyn"),
             uiOutput("energyDyn2")
    ),
    tabPanel("Candidate Locations"

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
      textInput("gmapsKey", "Please enter a Google Maps API key:", placeholder = "Insert key here...")
    } else {

    }
  })

  # output$busNotice <- renderText ({
  #   paste0("You indicated that ", input$buses, " bus types would be used.")
  # })

  busDefaultLabels <- reactive(paste0("Label", seq_len(input$buses)))

  virtBusTable <- reactiveValues(data = {
    data.frame("Label" = "Placeholder",
               "Cost" = 0,
               "Mass" = 1,
               "PackCapacity" = 1)
  })

  observeEvent(input$busTable_cell_edit, {
    info <- input$busTable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)+1
    if(j == 1) {
      k = as.character(info$value)
    } else {
      k = as.numeric(info$value)
    }
    virtBusTable$data[i,j] <- k
  })

  observeEvent(input$delRowBut, {
    req(nrow(virtBusTable$data) > 1 && nrow(virtBusTable$data) <= input$delRowSel)
    virtBusTable$data <- deleteRow(virtBusTable$data, input$delRowSel)
  })

  observeEvent(input$addRow, {
    virtBusTable$data <- addNewRow(virtBusTable$data)
  })

  output$busTable <- renderDT({
    DT::datatable(virtBusTable$data, editable = TRUE, rownames = FALSE)
  })

  output$testOut <- renderText(paste0("The name of the bus is ", virtBusTable$data$Label[1]))
}

shinyApp(ui, server)
