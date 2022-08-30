validERGExam <- function(object) {
  ret = 0
  if ((!(length(object@Steps_RAW) == length(unique(
    object@Measurements$Step
  ))) &&
  (length(unique(
    object@Measurements$Step
  )) == length(object@Steps_Timetrace)) &&
  length(object@Steps_RAW) != 0) ||
  (!(length(object@Steps_AVG) == length(unique(
    object@Measurements$Step
  ))) &&
  (length(unique(
    object@Measurements$Step
  )) == length(object@Steps_Timetrace)) &&
  length(object@Steps_AVG) != 0) ||
  (!(length(unique(
    object@Measurements$Step
  )) == dim(object@Stimulus)[1])) &&
  length(unique(object@Measurements$Step)) != 0) {
    ret <- ret + 1
    stop("Stimulus table incompatible with items in Steps or Measurements")
  }
  if (any(duplicated(object@Stimulus$Description))) {
    ret <- ret + 1
    stop(paste(
      "Descriptions provided not unique: ",
      object@Stimulus$Description[duplicated(object@Stimulus$Description)]
    ))
  }
  if (!length(object@Steps_AVG) == 0) {
    if (!length(object@Steps_AVG) == length(object@Steps_Excl)) {
      ret <- ret + 1
      stop("List of Average Traces and Excluded Traces not compatible")
    }
  }

  if (length(object@Steps_RAW) != 0 &&
      length(object@Steps_AVG) != 0) {
    if (!length(object@Steps_RAW) == length(object@Steps_AVG)) {
      ret <- ret + 1
      stop("List of Raw Traces and Average Traces not compatible")
    }
  }

  if (length(object@Steps_RAW) != 0 &&
      length(unique(object@Measurements$Step)) != 0) {
    if (!length(object@Steps_RAW) == length(unique(object@Measurements$Step))) {
      ret <- ret + 1
      stop("List of Raw Traces and Stats not compatible")
    }
  }

  if (!(length(
    colnames(object@Stimulus) %in% c("Description", "Intensity", "Background", "Type") ==
    4
  ))) {
    stop("Stimulus table malformatted")
  }

  if (object@ProtocolName == "") {
    stop("No Protocol Name provided")
  }

  if (object@ExamDate > object@Imported) { # was: as.POSIXct(object@Imported))
    stop(paste("Exam date is in future:", as.character(object@ExamDate),"<", as.character(object@Imported) ))
  }

  if (object@DOB > as.POSIXct(object@ExamDate)) {
    stop("Exam date before DOB.")
  }

  if (object@Filename == "") {
    stop("Filename not provided")
  }

  if (object@Patient == "") {
    stop("Patient name not provided")
  }

  if (ret == 0) {
    TRUE
  } else {
    FALSE
  }
}

#' An S4 class storing an ERG Exam
#'
#' This object is designed to store data and metadata from a single ERG experiment/exam
#'
#' @slot Measurements A data.frame containing results from measurements performed on ERG traces. Each row represents one measurement
#' @slot Channels A character vector containing the names of the recorded channels
#' @slot Stimulus A data.frame containing information on the stimuli employed. Each row represents one step.
#' @slot ProtocolName,Version,Filename,RecMode,Patient,Gender,Group,Investigator Character vectors.
#' @slot ExamDate,DOB,Imported POSIXct/Date containing time stamps of the time/date the examination was performed, the study subject was born and data were imported, resp.
#' @slot Steps_Timetrace,Steps_RAW,Steps_AVG,Steps_Excl Reserved for storing raw and averaged traced data. Currently unsupported.
#' @seealso \linkS4class{ERGExam}, \linkS4class{ImportEpsionMeasures}
#' @importFrom methods setClass new
#' @exportClass ERGExam
ERGExam <- setClass(
  Class = "ERGExam",
  slots =  list(
    Steps_Timetrace = "list",
    Steps_RAW = "list",
    Steps_AVG = "list",
    Steps_Excl = "list",
    Measurements = "data.frame",
    Channels = "character",
    Stimulus = "data.frame",
    ProtocolName = "character",
    Version = "character",
    ExamDate = "POSIXct",
    Filename = "character",
    RecMode = "character",
    Patient = "character",
    DOB = "Date",
    Gender = "character",
    Group = "character",
    Investigator = "character",
    Imported = "POSIXct"
  ),
  prototype = list("Imported" = as.POSIXct(Sys.time())),
  validity = validERGExam
)
