#' Explore ERGExam Metadata and Measurements
#'
#' Launches a Shiny app to explore the content of the Metadata slot of the ERGExam object and allows for filtering by metadata columns Eye and Channel as well as by the Stimulus columns Description, Intensity, Background, and Type. Metadata and Stimulus are matched by the Step column in Metadata, which contains the index of the corresponding row of the data.frame in the Stimulus slot.
#'
#' @param X An ERGExam object.
#' @return A Shiny app to interactively explore ERGExam metadata and measurements.
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel selectInput mainPanel h3 req observeEvent reactive stopApp onSessionEnded shinyApp runApp
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @export
#' @examples
#' data(ERG)
#' exploreERGExam(ERG)
setGeneric(
  name = "exploreERGExam",
  def = function(X) {
    standardGeneric("exploreERGExam")
  }
)

#' @noMd
setMethod(
  "exploreERGExam",
  "ERGExam",
  function(X) {

    ui <- fluidPage(
      titlePanel("Explore ERGExam Metadata"),
      sidebarLayout(
        sidebarPanel(
          selectInput("eye", "Eye:", choices = c("All", unique(X@Metadata$Eye))),
          selectInput("channel", "Channel:", choices = c("All", unique(X@Metadata$Channel))),
          selectInput("description", "Description:", choices = c("All", unique(X@Stimulus$Description))),
          selectInput("intensity", "Intensity:", choices = c("All", as.character(unique(X@Stimulus$Intensity)))),
          selectInput("background", "Background:", choices = c("All", unique(X@Stimulus$Background))),
          selectInput("type", "Type:", choices = c("All", unique(X@Stimulus$Type)))
        ),
        mainPanel(
          h3("Filtered Metadata"),
          dataTableOutput("filteredData"),
          h3("Measurements for Selected Metadata"),
          dataTableOutput("measurementsData")
        )
      )
    )

    server <- function(input, output, session) {
      filteredData <- reactive({
        metadata <- Metadata(X)
        stimulus <- Stimulus(X)
        measurements <- Measurements(X, TimesOnly = T, quiet = T)

        if (input$eye != "All") {
          metadata <- metadata[metadata$Eye == input$eye, ]
        }
        if (input$channel != "All") {
          metadata <- metadata[metadata$Channel == input$channel, ]
        }

        filteredStimulus <- stimulus
        if (input$description != "All") {
          filteredStimulus <- filteredStimulus[filteredStimulus$Description == input$description, ]
        }
        if (input$intensity != "All") {
          filteredStimulus <- filteredStimulus[filteredStimulus$Intensity == as.numeric(input$intensity), ]
        }
        if (input$background != "All") {
          filteredStimulus <- filteredStimulus[filteredStimulus$Background == input$background, ]
        }
        if (input$type != "All") {
          filteredStimulus <- filteredStimulus[filteredStimulus$Type == input$type, ]
        }

        filteredMetadata <- metadata[metadata$Step %in% filteredStimulus$Step, ]

        mergedData <- merge(filteredMetadata, filteredStimulus, by = "Step", all.x = TRUE)[,c("Channel","Eye","Intensity","Background","Type","Description","Result")]
        mergedData$IDX<-1:nrow(mergedData)
        measurementsCount <- as.data.frame(table(measurements$Recording))
        colnames(measurementsCount) <- c("IDX", "MeasurementsCount")
        # Merge the measurements count data frame with the mergedData data frame
        mergedData <- merge(mergedData, measurementsCount, by.x = "IDX", by.y = "IDX", all.x = TRUE)
        # Replace NA values in the Count column with 0
        mergedData$Count[is.na(mergedData$MeasurementsCount)] <- 0

        return(mergedData)
      })

      output$filteredData <- renderDataTable({
        filteredData()
      }, selection = "single",
      options = list(dom = 't'))

      output$measurementsData <- renderDataTable({
        req(input$filteredData_rows_selected)

        selectedRow <- as.numeric(row.names(filteredData()[input$filteredData_rows_selected, ]))
        selectedMeasurements <- measurements[measurements$Recording == selectedRow, c("Name","Relative","Time","Voltage")]

        datatable(selectedMeasurements)
      })

      # Observe the finish button
      observeEvent(input$finishBtn, {
        stopApp()
      })

      onSessionEnded(function() {
        stopApp()
      })

    }

    SetMarkersApp <- shinyApp(ui = ui, server = server)

    # run
    message("Waiting for Marker placement app to finish....")
    out <- runApp(SetMarkersApp, launch.browser = T)
    message("Finished. Please wait.")
  }
)
