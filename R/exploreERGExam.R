#' Explore ERGExam Metadata and Measurements
#'
#' Launches a Shiny app to explore the content of the Metadata slot of the ERGExam object and allows for filtering by metadata columns Eye and Channel as well as by the Stimulus columns Description, Intensity, Background, and Type. Metadata and Stimulus are matched by the Step column in Metadata, which contains the index of the corresponding row of the data.frame in the Stimulus slot.
#'
#' @param X An ERGExam object.
#' @return A Shiny app to interactively explore ERGExam metadata and measurements.
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel conditionalPanel selectInput checkboxInput mainPanel h4 hr req observeEvent reactive stopApp onSessionEnded shinyApp runApp
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @export
#' @examples
#' \dontrun{
#' data(ERG)
#' exploreERGExam(ERG)
#' }
setGeneric(
  name = "exploreERGExam",
  def = function(X) {
    standardGeneric("exploreERGExam")
  }
)


#' @noMd
setMethod("exploreERGExam",
          "ERGExam",
          function(X) {
            shiny.style <- HTML(
              "
                                    .highlight-row {
                                      border: 2px solid #3498db;
                                      padding: 10px;
                                      border-radius: 5px;
                                      margin: 10px 0;
                                    }
                                 "
            )

            ui <- fluidPage(
              tags$head(tags$style(shiny.style)),
              titlePanel("Explore ERGExam Metadata"),
              sidebarLayout(
                sidebarPanel(
                  h4("Filter recordings"),
                  selectInput("eye", "Eye:", choices = c("All", unique(X@Metadata$Eye))),
                  selectInput("channel", "Channel:", choices = c("All", unique(
                    X@Metadata$Channel
                  ))),
                  selectInput("description", "Description:", choices = c("All", unique(
                    X@Stimulus$Description
                  ))),
                  selectInput("intensity", "Intensity:", choices = c("All", as.character(
                    unique(X@Stimulus$Intensity)
                  ))),
                  selectInput("background", "Background:", choices = c("All", unique(
                    X@Stimulus$Background
                  ))),
                  selectInput("type", "Type:", choices = c("All", unique(X@Stimulus$Type))),
                  hr(),
                  h4("Settings"),
                  checkboxInput("showGraph", "Show Graph", value = FALSE),
                  width = 2
                ),
                mainPanel(
                  h4("Recordings stored in object"),
                  dataTableOutput("filteredData"),
                  hr(),
                  fluidRow(
                    column(
                      6,
                      h4("Measurements for selected recording"),
                      dataTableOutput("measurementsData"),
                    ),
                    column(
                      6,
                      conditionalPanel(
                        condition = "input.showGraph == true",
                        h4("Graph for selected recording"),
                        plotOutput("graphOutput")
                      )
                    )
                  )
                )
              )
            )

            metadata <- Metadata(X)
            stimulus <- Stimulus(X)
            measurements <-
              Measurements(X, TimesOnly = F, quiet = T)
            metadata$IDX <- 1:nrow(metadata)

            server <- function(input, output, session) {
              filteredData <- reactive({
                if (input$eye != "All") {
                  metadata <- metadata[metadata$Eye == input$eye,]
                }
                if (input$channel != "All") {
                  metadata <- metadata[metadata$Channel == input$channel,]
                }

                filteredStimulus <- stimulus
                if (input$description != "All") {
                  filteredStimulus <-
                    filteredStimulus[filteredStimulus$Description == input$description,]
                }
                if (input$intensity != "All") {
                  filteredStimulus <-
                    filteredStimulus[filteredStimulus$Intensity == as.numeric(input$intensity),]
                }
                if (input$background != "All") {
                  filteredStimulus <-
                    filteredStimulus[filteredStimulus$Background == input$background,]
                }
                if (input$type != "All") {
                  filteredStimulus <-
                    filteredStimulus[filteredStimulus$Type == input$type,]
                }

                filteredMetadata <-
                  metadata[metadata$Step %in% filteredStimulus$Step,]

                mergedData <-
                  merge(filteredMetadata,
                        filteredStimulus,
                        by = "Step",
                        all.x = TRUE)[, c(
                          "IDX",
                          "Channel",
                          "Eye",
                          "Intensity",
                          "Background",
                          "Type",
                          "Description",
                          "Result"
                        )]
                measurementsCount <-
                  as.data.frame(table(measurements$Recording))
                colnames(measurementsCount) <-
                  c("IDX", "MeasurementsCount")
                # Merge the measurements count data frame with the mergedData data frame
                mergedData <-
                  merge(
                    mergedData,
                    measurementsCount,
                    by.x = "IDX",
                    by.y = "IDX",
                    all.x = TRUE
                  )
                # Replace NA values in the Count column with 0
                mergedData$MeasurementsCount[is.na(mergedData$MeasurementsCount)] <-
                  0

                return(mergedData)
              })

              output$filteredData <- renderDataTable({
                filteredData()
              }, selection = "single",
              options = list(dom = 't'))

              output$measurementsData <- renderDataTable({
                req(input$filteredData_rows_selected)

                selectedRow <-
                  as.numeric(filteredData()[input$filteredData_rows_selected, "IDX"])
                selectedMeasurements <-
                  measurements[measurements$Recording == selectedRow, c("Name", "Relative", "Time", "Voltage")]

                datatable(selectedMeasurements)
              })

              output$graphOutput <- renderPlot({
                req(input$filteredData_rows_selected)
                req(input$showGraph)

                selectedRow <-
                  as.numeric(filteredData()[input$filteredData_rows_selected, "IDX"])
                ggERGTrace(X, where = list(Recording = selectedRow))
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

            # run)
            out <- runApp(SetMarkersApp, launch.browser = T)
          })
