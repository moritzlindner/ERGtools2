#' Interactively perform measurements on an ERGExam object
#'
#' This method allows interactive marker placement on \linkS4class{ERGExam} objects using  \link[shiny:shiny]{shiny:shiny-package}
#'
#' @inheritParams  Subset-method
#' @param downsample Use peak preserving downsampling, this may accelerate potting time for large objects.
#' @return An object of class 'ERGExam' with the new measurements added.
#' @export
#' @importFrom plotly ggplotly event_data plotlyOutput renderPlotly event_register
#' @importFrom shiny fluidPage titlePanel fluidRow column verticalLayout tags validate HTML h4 h5 actionButton selectInput textInput
#' @importFrom shiny modalDialog modalButton showModal removeModal observeEvent reactive stopApp runApp onSessionEnded shinyApp
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom stringr str_split str_detect
#' @importFrom ggplot2 aes geom_line geom_text facet_grid labs theme_minimal element_text geom_vline label_wrap_gen
#' @importFrom units set_units deparse_unit
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom tidyr %>%
#' @importFrom cli cli_warn cli_abort cli_alert_info cli_progress_bar cli_progress_update cli_progress_done
#' @examples
#' data(ERG)
#' ERG<-SetStandardFunctions(ERG)
#' \dontrun{
#' ERG<-interactiveMeasurements(ERG, Channel="OP",Eye="RE")
#' }
#' Measurements(ERG)
#' \dontrun{
#' ERG<-interactiveMeasurements(ERG, Channel="ERG")
#' }
#' Measurements(ERG)
#'
setGeneric(
  name = "interactiveMeasurements",
  def = function(X,
                 Channel = "ERG",
                 Eye = Eyes(X),
                 downsample = T) {
    standardGeneric("interactiveMeasurements")
  }
)

#' @noMd
setMethod("interactiveMeasurements",
          "ERGExam", {
            function(X,
                     Channel = "ERG",
                     Eye = Eyes(X),
                     downsample = T) {
              # internal functions
              shiny.style <- HTML(
                "
                                    .highlight-row {
                                      border: 2px solid #3498db;
                                      padding: 10px;
                                      border-radius: 5px;
                                      margin: 10px 0;
                                    }
                                    h4 {
                                      font-size: 1.1em;
                                      font-weight: bold;
                                      font-style: italic;
                                    }
                                    h5 {
                                      font-size: 1.1em;
                                      font-weight: bold;
                                    }
                                    label.control-label, .selectize-control.single{
                                      display: table-cell;
                                      text-align: left;
                                      vertical-align: middle;
                                    }
                                    .selectize-dropdown {
                                        width: 90px !important;
                                    }
                                    .selectize {
                                        width: 90px !important;
                                    }
                                    .form-group{
                                      display: table-row;
                                    }
                                 "
              )

              # Validity checks
              if (!all(Eye %in% Eyes(X))) {
                missing<-Eye[!(Eye %in% Eyes(X))]
                cli_abort(c("Invalid value(s) given for 'Eye'. Missing: {.val missing}"))
              }

              if (!all(Channel %in% Channels(X))) {
                missing<-Channel[!(Channel %in% Channels(X))]
                cli_abort(c("Invalid value given for 'Channel'. Missing: {.val missing}"))
              }

              if (length(Channel) != 1) {
                cli_abort(c("Exactly one valid Channel must be named. Is: {.val Channel}"))
              }

              stopifnot(CheckAvgFxSet(X))

              marker.sel.idx<-0

              #Get Relevant subset of the ERGExam and keep avg and filter fx.

              suppressMessages(curr <- Subset(X, Raw = F, skip.validation = T))
              if (downsample){
                curr@Data <- lapply(curr@Data,function(x){
                  trace <- as.data.frame(x)
                  times.keep<-trace$Time[which(diff(sign(diff(trace$Value))) %in% c(-2,2))+1]
                  suppressMessages(x<-Subset(x,Time = times.keep, TimeExclusive = T, skip.validation = T))
                })
              }

              # prepare ERGExam Object
              curr@Stimulus$Description <-
                iconv(curr@Stimulus$Description, "UTF-8", "UTF-8", sub = '')
              key <-
                as.factor(paste(
                  Metadata(curr)$Step,
                  Metadata(curr)$Eye,
                  Metadata(curr)$Repeat,
                  sep = "_"
                ))
              curr <- AddMetadata(curr, "key", key)



              jscode <- "shinyjs.closeWindow = function() { window.close(); }"
              # Define the UI for the application
              ui <- fluidPage(
                useShinyjs(),
                extendShinyjs(text = jscode, functions = c("closeWindow")),
                tags$head(tags$style(shiny.style)),
                titlePanel(
                  paste0(
                    "Set Markers - Subject: ",
                    Subject(curr),
                    ", DOE: ",
                    ExamDate(curr)
                  )
                ),
                sidebarLayout(
                  sidebarPanel(
                    verticalLayout(
                      fluidRow(class = "highlight-row",
                               h4("Channels"),
                               selectInput(
                                 "INP_channel_sel",
                                 "Active Channel",
                                 choices = c(Channels(curr)),
                                 width = '70px',
                                 selected = which(Channels(X) == Channel)
                               )),
                      fluidRow(
                        class = "highlight-row",
                        h4("Markers"),
                        verticalLayout(
                          fluidRow(
                            class = "highlight-row",
                            h5("Select active marker"),
                            dataTableOutput("markerTable")
                          ),
                          fluidRow(
                            class = "highlight-row",
                            actionButton("INP_marker_delete", "Delete active marker")
                          ),
                          fluidRow(
                            class = "highlight-row",
                            h5("Create new marker"),
                            textInput("INP_marker_new",
                                      "Name",
                                      width = '100%'),
                            selectInput(
                              "INP_marker_new_rel",
                              "Relative to",
                              choices = c("", Markers(curr)$Name[Markers(curr)$Channelbinding == Channels(curr)[1]]),
                              width = '100%',
                              selected = 1
                            ),
                            actionButton("INP_marker_new_add", "<-- Add new marker")
                          )
                        )
                      ),
                      fluidRow(class = "highlight-row",
                               actionButton("finishBtn", "Finish"))
                    ),
                    width = 3
                  ),
                  mainPanel(
                    h4("Traces"),
                    plotlyOutput(
                      "trace_plot",
                      height =  paste0(200 + (100 * length(Steps(
                        curr
                      ))), "px"),
                      width = paste0(120 + (300 * length(Eyes(
                        curr
                      ))), "px")
                    ),
                    hr(),
                    h4("Measurements"),
                    dataTableOutput("measurement_output"),
                    actionButton("INP_measurement_delete", "Delete selected measurement"),
                    width = 9
                  ),
                  position = "right"
                )
              )

              # Define the server for the application
              server <- function(input, output, session) {

                out <- curr
                CURR <- reactiveValues(data = curr)

                # Render the interactive plot
                # output$trace_plot <- renderPlotly({
                #   gglotly_wrapper(CURR$data, input$INP_channel_sel, downsample = downsample) %>%
                #     event_register("plotly_click")
                # })

                #Display the available markers
                output$markerTable <-
                  renderDataTable({
                    Markers(CURR$data)[, c("Name", "Relative")]
                  }, selection = "single",
                  options = list(dom = 't'))

                # Observe marker Select
                observeEvent(input$markerTable_rows_selected, {
                  existing <- currentmarkers(CURR$data,input$INP_channel_sel)
                  marker.sel.idx <<- isolate(input$markerTable_rows_selected)
                  curr_marker <<-
                    existing$Name[marker.sel.idx]
                  curr_marker_ref <<-
                    existing$Relative[marker.sel.idx]
                })

                # Observe measurement Select
                observeEvent(input$measurement_output_rows_selected, {
                  measurement.sel.idx <<- isolate(input$measurement_output_rows_selected)
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
                                   if (!is.null(click.traceinfo)) {
                                     click.Step <-
                                       as.numeric(click.traceinfo[1])
                                     click.Eye <- click.traceinfo[2]
                                     click.Repeat <-
                                       as.numeric(click.traceinfo[3])
                                     click.idx <-
                                       IndexOf(
                                         CURR$data,
                                         Step = click.Step,
                                         Eye = click.Eye,
                                         Channel = isolate(input$INP_channel_sel),
                                         Repeat = click.Repeat
                                       )

                                     existing <- currentmarkers(CURR$data,input$INP_channel_sel)
                                     canupdate = F
                                     if (!is.na(existing$Relative[existing$Name == curr_marker])) {
                                       if (nrow(
                                         Measurements(
                                           X = CURR$data,
                                           Marker = existing$Relative[existing$Name ==
                                                                      curr_marker],
                                           where = click.idx,
                                           TimesOnly = T
                                         )
                                       ) != 0) {
                                         canupdate = T
                                       }
                                     } else {
                                       canupdate = T
                                     }

                                     if (canupdate) {
                                       Measurements(
                                         X = CURR$data,
                                         Marker = curr_marker,
                                         where = click.idx,
                                         create.marker.if.missing = F,
                                         Relative = NULL,
                                         ChannelBinding = NULL,
                                         skip.validation = T
                                       ) <-
                                         as_units(click.coord$x, deparse_unit(TimeTrace(curr@Data[[click.idx]])))
                                     } else {
                                       showModal(
                                         modalDialog(
                                           title = "Placement of marker failed.",
                                           paste0(
                                             "The marker ",
                                             curr_marker,
                                             " could not be placed on this trace. This might be becuase the marker is relative to another, which is still missing in the trace."
                                           ),
                                           easyClose = TRUE,
                                           footer = NULL
                                         )
                                       )
                                     }

                                     # update the plot
                                     output$trace_plot <-
                                       renderPlotly({
                                         gglotly_wrapper(CURR$data, input$INP_channel_sel, downsample = downsample)
                                       })
                                     # reselect marker table
                                     output$markerTable <- markerTable_wrapper(CURR$data, input$INP_channel_sel, marker.sel.idx)
                                     # update measurements table

                                     output$measurement_output <- measurementTable_wrapper(CURR$data)

                                     out <<-
                                       reactiveValuesToList(CURR)$data
                                   }
                                 }
                               }
                             })
                # Observe Channel Select
                observeEvent(input$INP_channel_sel, {
                  active_channel <- isolate(input$INP_channel_sel)
                  if (!is.null(active_channel) && active_channel %in% Channels(CURR$data)){
                    # update the plot
                    output$trace_plot <-
                      renderPlotly({
                        gglotly_wrapper(CURR$data, input$INP_channel_sel, downsample = downsample)
                      })
                    # reselect marker table
                    output$markerTable <- markerTable_wrapper(CURR$data, input$INP_channel_sel, marker.sel.idx)

                    # update relative markers
                    updateSelectInput(session,
                                      "INP_marker_new_rel",
                                      choices = c("", existing <- currentmarkers(CURR$data,input$INP_channel_sel)$Name))

                    output$measurement_output <- measurementTable_wrapper(CURR$data)

                  } else {
                    showModal(
                      modalDialog(
                        title = "Selecting active Channel failed.",
                        paste0("The selected channel ", active_channel, " could not found in the object "),
                        easyClose = TRUE,
                        footer = NULL
                      )
                    )
                  }
                })

                # Observe new Markers
                observeEvent(input$INP_marker_new_add, {
                  new_marker <- isolate(input$INP_marker_new)
                  new_marker_rel <-
                    isolate(input$INP_marker_new_rel)
                  if (!is.null(new_marker) &&
                      new_marker != "" &&
                      !(new_marker %in% Markers(CURR$data)$Name)) {
                    CURR$data <-
                      AddMarker(
                        CURR$data,
                        Marker = new_marker,
                        Relative = which(Markers(CURR$data)$Name == new_marker_rel),
                        ChannelBinding = isolate(input$INP_channel_sel)
                      )
                    updateSelectInput(session,
                                      "INP_marker_new_rel",
                                      choices = c("", existing <- currentmarkers(CURR$data,input$INP_channel_sel)$Name))
                    # reselect marker table
                    output$markerTable <- markerTable_wrapper(CURR$data, input$INP_channel_sel, marker.sel.idx)

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
                  showModal(modalDialog(
                    title = "Delete Marker",
                    paste0("Delete selected marker(", curr_marker, ") and all markers relative to it?"),
                    footer = tagList(
                      actionButton("confirmDelete", "Delete"),
                      modalButton("Cancel")
                    )
                  ))
                })
                observeEvent(input$confirmDelete, {
                  CURR$data <-
                    DropMarker(
                      CURR$data,
                      curr_marker,
                      ChannelBinding = isolate(input$INP_channel_sel),
                      drop.dependent = T
                    )
                  out <<- reactiveValuesToList(CURR)$data
                  removeModal()
                })

                # Observe delete Measurement Button
                observeEvent(input$INP_measurement_delete, {
                  if (!is.null(measurement.sel.idx)) {
                    if (all(measurement.sel.idx %in% 1:nrow(CURR$data@Measurements@Measurements))) {
                      # needs to implement checks to ensure data dropped have no depending or call Measurements()<-NULL

                      tryCatch({
                        measurement.sel.row <- Measurements(CURR$data, quiet = T, TimesOnly = T)[measurement.sel.idx,]
                        Measurements(
                          CURR$data,
                          where = list(
                            Channel = measurement.sel.row$Channel,
                            Step = measurement.sel.row$Step,
                            Eye = measurement.sel.row$Eye,
                            Repeat = measurement.sel.row$Repeat
                          ),
                          Marker = measurement.sel.row$Name,
                          ChannelBinding = isolate(input$INP_channel_sel)
                        ) <- NULL

                        output$measurement_output <- measurementTable_wrapper(CURR$data)
                      },
                        error = function(e) {
                          if (str_detect(as.character(e), "because other measurements depend on it")) {
                            e <- "Other measurements depend on the measurement you are trying to delete. "
                          }
                          showModal(
                            modalDialog(
                              title = "Deleting measurement failed.",
                              paste0("Deleting measurement failed with message: ",e),
                              easyClose = TRUE,
                              footer = NULL
                            )
                          )
                        }
                      )
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
                  showModal(modalDialog(
                    title = "Close & Save",
                    "You will now close the dialog and return an updated ERGExam object",
                    footer = tagList(
                      actionButton("confirmSaveClose", "Close & Save"),
                      actionButton("confirmDiscard", "Close & DISCARD changes"),
                    )
                  ))
                })
                observeEvent(input$confirmSaveClose, {
                  js$closeWindow()
                  stopApp(out)
                })

                observeEvent(input$confirmDiscard, {
                  js$closeWindow()
                  stopApp(curr)
                })
                onSessionEnded(function() {
                  js$closeWindow()
                  stopApp(out)
                })

                #Display the measurement results
                output$measurement_output <- measurementTable_wrapper(CURR$data)
              }

              # Make the application
              SetMarkersApp <- shinyApp(ui = ui, server = server)

              # run
              message("Waiting for Marker placement app to finish....")
              out <- runApp(SetMarkersApp, launch.browser = T)
              message("Finished. Updating values. Please wait.")


              X@Measurements<-out@Measurements

              ## Validate
              if (validObject(X)) {
                X<-LogChange(X)
                return(X)
              }
            }
          })

#' Internal fx for plotting interactive plots in interactiveMeasurements
#' @keywords Internal
#' @noMd
#' @noRd
interact_plot <- function(erg.obj, channel, downsample = downsample, skip.validation = T) {
  suppressMessages(erg.obj <- Subset(erg.obj, where = list (Channel = channel), skip.validation = skip.validation))
  trace <- as.data.frame(erg.obj)
  trace <-
    merge(trace, Stimulus(erg.obj)[c("Step", "Description")], by = "Step")
  trace$Value <- set_units(trace$Value, "uV")
  trace$Description <- factor(trace$Description, levels = unique(trace$Description[order(trace$Step)]),
                              ordered = TRUE)

  markers <-
    Measurements(erg.obj, measure.absolute = T)
  colnames(markers)[colnames(markers) == "Voltage"] <-
    "Value"

  g <- ggplot(trace,
              aes(
                x = Time,
                y = Value,
                color = factor(Repeat),
                customdata = key
              )) +
    geom_line()
  if (nrow(markers) > 0) {
    g <- g +
      geom_vline(data = markers,
                 aes(xintercept = Time),
                 alpha = 0.1) +
      geom_text(data = markers,
                aes(
                  x = Time,
                  y = Value,
                  label = Name
                ),
                color = "black")
  }
  g <- g +
    facet_grid(Description ~ Eye,
               scales = "free_y",
               labeller = label_wrap_gen(25)) +
    labs(title = "Trace Plots", x = "Time", y = "Amplitude") +
    theme_minimal() +
    theme(legend.position = "none",
          strip.text.y = element_text(angle = -90)) # Multi-ine labeller https://stackoverflow.com/questions/12673392/how-to-fit-strip-text-x-if-the-heading-string-is-too-long
  return(g)
}

#' Internal fx for plotly in interactiveMeasurements
#' @keywords Internal
#' @importFrom plotly ggplotly
#' @noMd
#' @noRd
gglotly_wrapper <- function(X, inp, downsample) {
  ggplotly(interact_plot(X, channel = isolate(inp), downsample = downsample),
           width = (80 + (300 * length(Eyes(
             X
           )))))
}

#' Internal fx for plotly in interactiveMeasurements
#' @keywords Internal
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @noMd
#' @noRd
markerTable_wrapper <- function (X, inp, sel) {
  renderDataTable({
    currentmarkers(X,inp)
  }, selection = list(mode = "single",
                      selected = sel),
  options = list(dom = 't'), rownames = F)
}

#' Internal fx for plotly in interactiveMeasurements
#' @keywords Internal
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @noMd
#' @noRd
measurementTable_wrapper <- function (X) {
  renderDataTable({
    mes <-
      Measurements(X, TimesOnly = T, quiet = T)[, c("Step",
                                                            "Description",
                                                            "Channel",
                                                            "Eye",
                                                            "Repeat",
                                                            "Name",
                                                            "Time")]
    mes
  }, width = "100%" , selection = "single",
  options = list(paging = 't', searching = 't', pageLength = 10), rownames = F)
}

currentmarkers <- function (X, inp) {
  marker.df <- Markers(X)[, c("Name", "Relative", "ChannelBinding")]
  marker.df <-
    marker.df[marker.df$ChannelBinding == isolate(inp), c("Name", "Relative")]
  marker.df
}
