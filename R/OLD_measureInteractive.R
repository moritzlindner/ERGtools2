# # Install required packages if not installed
# # install.packages(c("shiny", "plotly"))
#
# library(shiny)
# library(plotly)
#
# # Create a sample data frame (replace this with your actual data)
# trace_data <- data.frame(
#   time = seq(0, 10, 0.1),
#   amplitude = sin(seq(0, 10, 0.1))
# )
#
# # Define the UI for the application
# ui <- fluidPage(
#   titlePanel("Interactive Trace Measurement"),
#
#   # Display the interactive plot
#   plotlyOutput("trace_plot"),
#
#   # Display the measurement results
#   verbatimTextOutput("measurement_output")
# )
#
# # Define the server for the application
# server <- function(input, output) {
#
#   # Reactive variable to store clicked values
#   clicked_values <- reactiveValues(time = NULL, amplitude = NULL)
#
#   # Render the interactive plot
#   output$trace_plot <- renderPlotly({
#     plot_ly(data = trace_data, x = ~time, y = ~amplitude, type = 'scatter', mode = 'lines+markers') %>%
#       event_register("plotly_click")  # Enable click event
#   })
#
#   # Update the reactive variable and highlight the clicked point
#   observeEvent(event_data("plotly_click"), {
#     clicked_data <- event_data("plotly_click")
#     if (!is.null(clicked_data)) {
#       clicked_values$time <- clicked_data$x
#       clicked_values$amplitude <- clicked_data$y
#
#       # Highlight the clicked point by changing its color
#       output$trace_plot <- renderPlotly({
#         plot_ly(data = trace_data, x = ~time, y = ~amplitude, type = 'scatter', mode = 'lines+markers') %>%
#           add_trace(x = clicked_values$time, y = clicked_values$amplitude, type = 'scatter', mode = 'markers', marker = list(color = 'red')) %>%
#           layout(title = "Interactive Trace Measurement")
#       })
#     }
#   })
#
#   # Display the measurement results
#   output$measurement_output <- renderText({
#     paste("Time: ", clicked_values$time, "\nAmplitude: ", clicked_values$amplitude)
#   })
# }
#
# # Run the application
# shinyApp(ui = ui, server = server)
