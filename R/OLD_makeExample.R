#' #' Create Example ERGSteps Object
#' #'
#' #' This function creates an example ERGSteps object with mock data and metadata.
#' #'
#' #' @return An instance of the ERGSteps class containing example data and metadata.
#' #' @importFrom methods new
#' #'
#' #' @examples
#' #' example_erg_steps <- makeExampleERGSteps()
#' #'
#' #' @export
#' makeExampleERGSteps <- function() {
#'   example_data <-
#'     list(
#'       makeExampleEPhysData(sample_points = 400, replicate_count = 9),
#'       makeExampleEPhysData(sample_points = 400, replicate_count = 9),
#'       makeExampleEPhysData(sample_points = 400, replicate_count = 9),
#'       makeExampleEPhysData(sample_points = 400, replicate_count = 9),
#'       makeExampleEPhysData(sample_points = 600, replicate_count = 4),
#'       makeExampleEPhysData(sample_points = 600, replicate_count = 4),
#'       makeExampleEPhysData(sample_points = 600, replicate_count = 4),
#'       makeExampleEPhysData(sample_points = 600, replicate_count = 4),
#'       makeExampleEPhysData(sample_points = 400, replicate_count = 50),
#'       makeExampleEPhysData(sample_points = 400, replicate_count = 50),
#'       makeExampleEPhysData(sample_points = 400, replicate_count = 50),
#'       makeExampleEPhysData(sample_points = 400, replicate_count = 50)
#'     )
#'
#'   # Generate random "Step" and "Channel" entries
#'   num_entries <- length(example_data)
#'
#'   example_metadata <- data.frame(
#'     Step = paste0("Step_", ceiling(seq(1,12)/4)),
#'     Eye = rep(c("OD","OS"),6),
#'     Channel = rep(c("ERG","ERG","OP","OP"),3),
#'     stringsAsFactors = FALSE
#'   )
#'   ExamInfo <- list()
#'   ExamInfo$ProtocolName <- "RandomData"
#'   ExamInfo$Version <- "1"
#'   ExamInfo$ExamDate <- date()
#'   ExamInfo$Filename <- "Nofile"
#'   ExamInfo$RecMode <- "ERG Test"
#'   ExamInfo$Investigator <- "MOL"
#'
#'
#'   ERGExam <- newERGExam(
#'                  Data = example_data,
#'                  Metadata = example_metadata,
#'                  Measurements = data.frame(Recording=NULL,Name=NULL,Time=NULL,Voltage=NULL),
#'                  )
#'
#'   return(erg_steps)
#' }
