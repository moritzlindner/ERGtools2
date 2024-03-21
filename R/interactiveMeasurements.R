#' Interactively perform measurements on an ERGExam object
#'
#' This method allows interactive marker placement on \linkS4class{ERGExam} objects using  \link[shiny:shiny]{shiny:shiny-package}
#'
#' @inheritParams  Subset-method
#' @return An object of class 'ERGExam' with the new measurements added.
#' @export
#' @importFrom plotly ggplotly event_data plotlyOutput renderPlotly event_register
#' @importFrom shiny fluidPage titlePanel fluidRow column verticalLayout
#' @importFrom shiny tags validate HTML h4
#' @importFrom shiny actionButton selectInput textInput
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom shiny modalDialog modalButton showModal removeModal
#' @importFrom shiny observeEvent reactive
#' @importFrom shiny stopApp runApp onSessionEnded shinyApp
#' @importFrom stringr str_split
#' @importFrom ggplot2 aes geom_line geom_text facet_grid labs theme_minimal element_text
#' @importFrom units set_units deparse_unit
#' @importFrom tidyr %>%
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @examples
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' \dontrun{
#' ERG<-interactiveMeasurements(ERG)
#' }
#' Measurements(ERG)
#' \dontrun{
#' ERG<-interactiveMeasurements(ERG)
#' }
#' Measurements(ERG)
#'
setGeneric(
  name = "interactiveMeasurements",
  def = function(X,
                 Channel = "ERG",
                 Eye = Eyes(X)) {
    standardGeneric("interactiveMeasurements")
  }
)

#' @noMd
setMethod("interactiveMeasurements",
          "ERGExam", {
            function(X,
                     Channel = "ERG",
                     Eye = Eyes(X)) {

              # internal functions
              interact_plot <- function(erg.obj) {
                trace <- as.data.frame(erg.obj)
                trace$Value<-set_units(trace$Value, "uV")
                markers <- Measurements(erg.obj@Measurements)
                markers$ChannelBinding<-NULL
                markers <- merge(markers,Metadata(erg.obj),by.x="Recording",by.y = 0)
                markers <- merge(markers,StimulusTable(erg.obj)[c("Step","Description")], by="Step")
                trace <- merge(trace, StimulusTable(erg.obj)[c("Step","Description")], by = "Step")
                units_options(set_units_mode = "standard")
                markers$Time<-set_units(markers$Time,deparse_unit(trace$Time))
                markers<-merge(markers,trace, all.x = T, all.y=F)

                markers <- ConvertMeasurementsToAbsolute(markers)

                g <- ggplot(trace,
                            aes(
                              x = Time,
                              y = Value,
                              color = factor(Result),
                              customdata = key
                            )) +
                  geom_line() +
                  geom_text(data = markers,
                            aes(x = Time,
                                y = Value,
                                label = Name),
                            color = "black") +
                  facet_grid(Description ~ Eye, scales = "free_y") +
                  labs(title = "Trace Plots", x = "Time", y = "Amplitude") +
                  theme_minimal() +
                  theme(legend.position = "none",
                        strip.text.y = element_text(angle = -85))
                return(g)
              }

              shiny.style <- HTML("
                                    .highlight-row {
                                      border: 2px solid #3498db;
                                      padding: 10px;
                                      border-radius: 5px;
                                      margin: 10px 0;
                                    }
                                 ")

              # Validity checks
              if (!all(Eye %in% Eyes(X))) {
                stop("Invalid value(s) given for 'Eye'.")
              }

              if (!all(Channel %in% Channels(X))) {
                stop("Invalid value given for 'Channel'.")
              }

              if(length(Channel)!=1){
                stop("Exactly one valid Channel must be named")
              }

              stopifnot(CheckAvgFxSet(X))

              #keep the measurements from all Channels/Eyes not (re)measured now
              Measurements.unchanged <-
                IndexOf(X)[!(IndexOf(X) %in% IndexOf(X, Eye = Eye, Channel = Channel))]

              #Get Relevant subset of the ERGExam and keep avg and filter fx.

              curr <- Subset(X,
                             which = list(
                               Channel = Channel,
                               Eye =  Eye
                             ),
                             Raw = F)


              markers <- Markers(curr)
              for (i in 1:nrow(markers)) {
                markers$ChannelBinding[i]
                if (!(markers$ChannelBinding[i] %in% Channel)) {
                  curr <- DropMarker(curr,
                                     markers$Name[i],
                                     markers$ChannelBinding[i],
                                     drop.dependent = T)
                }
              }

              # prepare ERGExam Object
              curr@Stimulus$Description <-
                iconv(curr@Stimulus$Description, "UTF-8", "UTF-8", sub = '')
              key <-
                as.factor(paste(
                  Metadata(curr)$Step,
                  Metadata(curr)$Eye,
                  Metadata(curr)$Result,
                  sep = "_"
                ))
              curr <- AddMetadata(curr, "key", key)


              # Define the UI for the application
              ui <- fluidPage(
                tags$head(tags$style(shiny.style)),
                titlePanel(
                  paste0(
                    "Set Markers - Subject: ",
                    Subject(X),
                    ", DOE: ",
                    ExamDate(X),
                    ", Channel = ",
                    Channel
                  )
                ),
                fluidRow(
                  class = "highlight-row",
                  column(8,
                         plotlyOutput(
                           "trace_plot", height =  paste0(100 * length(Steps(curr)), "px")
                         )),
                  column(
                    4,
                    verticalLayout(
                      fluidRow(class = "highlight-row",
                               actionButton("finishBtn", "Finish")),
                      fluidRow(
                        class = "highlight-row",
                        h4("Select marker to place"),
                        dataTableOutput("markerTable")
                      ),
                      fluidRow(
                        class = "highlight-row",
                        h4("Add new marker"),
                        textInput("INP_marker_new", "Name"),
                        selectInput(
                          "INP_marker_new_rel",
                          "Relative to",
                          choices = c("", markers$Name),
                          selected = 1
                        ),
                        actionButton("INP_marker_new_add", "<-- Add new marker")
                      ),

                      fluidRow(
                        class = "highlight-row",
                        actionButton("INP_marker_delete", "Delete selected marker")
                      )
                    )
                  )
                ),
                fluidRow(class = "highlight-row",
                         column(
                           12,
                           h4("Measurements"),
                           dataTableOutput("measurement_output"),
                           actionButton("INP_measurement_delete", "Delete selected measurement")
                         ))
              )

              # Define the server for the application
              server <- function(input, output, session) {
                out <- curr
                CURR <- reactiveValues(data = curr)

                # Render the interactive plot
                output$trace_plot <- renderPlotly({
                  ggplotly(interact_plot(CURR$data)) %>%
                    event_register("plotly_click")
                })

                #Display the available markers
                output$markerTable <-
                  renderDataTable({
                    Markers(CURR$data)[, c("Name", "Relative")]
                  }, selection = "single",
                  options = list(dom = 't'))

                # Observe marker Select
                observeEvent(input$markerTable_rows_selected, {
                  marker.sel.idx <<- isolate(input$markerTable_rows_selected)
                  curr_marker <<-  Markers(CURR$data)$Name[marker.sel.idx]
                  curr_marker_ref <<-
                    Markers(CURR$data)$Relative[marker.sel.idx]
                })

                # Observe measurement Select
                observeEvent(input$measurement_output_rows_selected, {
                  measurement.sel.idx <<-
                    isolate(input$measurement_output_rows_selected)
                })

                # Observe new marker position set
                observeEvent(event_data("plotly_click"),
                             {
                               click.coord <- event_data("plotly_click")
                               if (!is.null(click.coord)) {
                                 if (curr_marker != "") {
                                   # get current eye and step for curve_number
                                   click.traceinfo <-
                                     unlist(stringr::str_split(click.coord$customdata, "_"))
                                   click.Step <- as.numeric(click.traceinfo[1])
                                   click.Eye <- click.traceinfo[2]
                                   click.Result <- as.numeric(click.traceinfo[3])
                                   click.idx <-
                                     IndexOf(
                                       CURR$data,
                                       Step = click.Step,
                                       Eye = click.Eye,
                                       Channel = Channel,
                                       Result = click.Result
                                     )

                                   Measurements(
                                     X = CURR$data,
                                     Marker = curr_marker,
                                     where = click.idx,
                                     create.marker.if.missing = F,
                                     Relative = NULL,
                                     ChannelBinding = NULL
                                   ) <-
                                     as_units(click.coord$x, deparse_unit(TimeTrace(curr@Data[[click.idx]])))

                                   # update the plot
                                   output$trace_plot <- renderPlotly({
                                     ggplotly(interact_plot(CURR$data))
                                   })
                                   # reselect marker table
                                   output$markerTable <-
                                     renderDataTable({
                                       Markers(CURR$data)[, c("Name", "Relative")]
                                     }, selection = list(mode = "single",
                                                         selected = marker.sel.idx),
                                     options = list(dom = 't'))

                                   out <<- reactiveValuesToList(CURR)$data

                                 }
                               }
                             })

                # Observe new Markers
                observeEvent(input$INP_marker_new_add, {
                  new_marker <- isolate(input$INP_marker_new)
                  new_marker_rel <- isolate(input$INP_marker_new_rel)
                  if (!is.null(new_marker) &&
                      new_marker != "" && !(new_marker %in% Markers(CURR$data)$Name)) {
                    CURR$data <-
                      AddMarker(
                        CURR$data,
                        Marker = new_marker,
                        Relative = which(Markers(CURR$data)$Name == new_marker_rel),
                        ChannelBinding = Channel
                      )
                    out <<- reactiveValuesToList(CURR)$data
                  } else {
                    showModal(
                      modalDialog(
                        title = "Adding of new marker failed.",
                        paste0("The new marker could not be added ",
                               if (new_marker == "") {
                                 ": New marker name must not be an empty string,"
                               },
                               if (new_marker %in% Markers(CURR$data)$Name) {
                                 ": Marker name already in list."
                               }),
                        easyClose = TRUE,
                        footer = NULL
                      )
                    )
                  }
                })

                # Observe deleteMarker Button
                observeEvent(input$INP_marker_delete, {
                  showModal(
                    modalDialog(
                      title = "Delete Marker",
                      "Delete selected marker?",
                      footer = tagList(
                        actionButton("confirmDelete", "Delete"),
                        modalButton("Cancel")
                      )
                    )
                  )
                })
                observeEvent(input$confirmDelete, {
                  CURR$data <-
                    DropMarker(
                      CURR$data,
                      curr_marker,
                      ChannelBinding = Channel,
                      drop.dependent = T
                    )
                  out <<- reactiveValuesToList(CURR)$data
                  removeModal()
                })

                #Observe delete Measurement Button
                observeEvent(input$INP_measurement_delete, {
                  if (!is.null(measurement.sel.idx)) {
                    if (measurement.sel.idx %in% 1:nrow(CURR$data@Measurements@Measurements)) {
                      warning("not yet implemented")
                      print (measurement.sel.idx)
                      print(CURR$data@Measurements@Measurements[measurement.sel.idx, ])

                      # rownames(CURR$data@Measurements@Measurements) <- NULL
                      # CURR$data@Measurements@Measurements[measurement.sel.idx, ] <- NULL
                      # rownames(CURR$data@Measurements@Measurements) <- NULL
                      out <<- reactiveValuesToList(CURR)$data
                    } else {
                      showModal(
                        modalDialog(
                          title = "Deleting Measurement failed.",
                          "No Measurement was selected.",
                          easyClose = TRUE,
                          footer = NULL
                        )
                      )
                    }
                  } else {
                    showModal(
                      modalDialog(
                        title = "Deleting Measurement failed.",
                        "Selection does not exist.",
                        easyClose = TRUE,
                        footer = NULL
                      )
                    )
                  }
                })

                # Observe the finish button
                observeEvent(input$finishBtn, {
                  stopApp(out)
                })

                onSessionEnded(function() {
                  stopApp(out)
                })

                #Display the measurement results
                output$measurement_output <-
                  renderDataTable({
                    tmp <- merge(
                      Metadata(CURR$data)[, colnames(Metadata(curr) != "key")],
                      Measurements(CURR$data@Measurements),
                      by = 0,
                      by.y = "Recording"
                    )
                    datatable(tmp[with(tmp, order(Step, Eye, Result)), ])

                  }, width = "100%", selection = "single",
                  options = list(dom = 't'))

              }


              # Make the application
              SetMarkersApp <- shinyApp(ui = ui, server = server)

              # run
              message("Waiting for Marker placement app to finish....")
              out <- runApp(SetMarkersApp, launch.browser = T)
              message("Finished. Please wait.")

              # merge measurements with unchanged part of object
              old.measurements <- Measurements(curr@Measurements)
              new.measurements <- Measurements(out@Measurements)
              removed.measurements <-
                old.measurements[!(do.call(paste, old.measurements[, c("Recording", "Name", "ChannelBinding", "Relative")]) %in% do.call(paste, new.measurements[, c("Recording", "Name", "ChannelBinding", "Relative")])), c("Recording", "Name", "ChannelBinding")]
              ## update and add
              new.measurements <-
                merge(
                  Measurements(out@Measurements),
                  Metadata(out),
                  by.x = "Recording",
                  by.y = 0
                )

              message("Updating measurements...")
              pb = txtProgressBar(min = 0,
                                  max = nrow(new.measurements),
                                  initial = 0)
              for (m in 1:nrow(new.measurements)) {
                curr.m <- new.measurements[m, ]
                Measurements(
                  X,
                  Marker = curr.m$Name,
                  where = Where(X,where=list(
                    Step = curr.m$Step,
                    Eye = curr.m$Eye,
                    Channel = curr.m$ChannelBinding,
                    Result = curr.m$Result
                  )),
                  create.marker.if.missing = T,
                  Relative = curr.m$Relative,
                  ChannelBinding = curr.m$ChannelBinding
                ) <- curr.m$Time
                setTxtProgressBar(pb,m)
              }
              close(pb)

              ## drop removed
              if (nrow(removed.measurements)>0){
                message("Deleting removed measurements...")
                pb = txtProgressBar(min = 0,
                                    max = nrow(new.measurements),
                                    initial = 0)
                for (m in 1:nrow(removed.measurements)) {
                  curr.m <- removed.measurements[m, ]
                  Measurements(
                    X,
                    Marker = curr.m$Name,
                    where = Where(X,where=list(
                      Step = curr.m$Step,
                      Eye = curr.m$Eye,
                      Channel = curr.m$ChannelBinding,
                      Result = curr.m$Result
                    )),
                    create.marker.if.missing = T,
                    Relative = curr.m$Relative,
                    ChannelBinding = curr.m$ChannelBinding
                  ) <- NULL
                  setTxtProgressBar(pb,m)
                }
                close(pb)
              }
              if (validObject(X)) {
                return(X)
              }
            }
          }
)
