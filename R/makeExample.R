#' Create Example ERGSteps Object
#'
#' This function creates an example ERGSteps object with mock data and metadata.
#'
#' @return An instance of the ERGSteps class containing example data and metadata.
#' @importFrom methods new
#'
#' @examples
#' example_erg_steps <- makeExampleERGSteps()
#'
#' @export
makeExampleERGSteps <- function() {
  example_data <-
    list(
      makeExampleEPhysData(),
      makeExampleEPhysData(),
      makeExampleEPhysData(),
      makeExampleEPhysData(),
      makeExampleEPhysData(),
      makeExampleEPhysData()
    )

  # Generate random "Step" and "Channel" entries
  num_entries <- length(example_data)
  random_steps <- paste0("Step", sample(1:10, num_entries, replace = TRUE))
  random_channels <- paste0("Ch_", sample(1:5, num_entries, replace = TRUE))

  example_metadata <- data.frame(
    Step = random_steps,
    Eye = c("LE", "RE","LE", "RE","LE", "RE"),
    Channel = random_channels,
    stringsAsFactors = FALSE
  )

  erg_steps <- new("ERGSteps",
                   Data = example_data,
                   Metadata = example_metadata)

  return(erg_steps)
}
