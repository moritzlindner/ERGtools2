#' #' @noMd
#' validERGExam <- function(object) {
#'   ret = 0
#'   warning("validity checks must be updated when raw data included")
#'   # if ((!(length(object@Steps_RAW) == length(unique(
#'   #   object@Measurements$Step
#'   # ))) &&
#'   # (length(unique(
#'   #   object@Measurements$Step
#'   # )) == length(object@Steps_Timetrace)) &&
#'   # length(object@Steps_RAW) != 0) ||
#'   # (!(length(object@Steps_AVG) == length(unique(
#'   #   object@Measurements$Step
#'   # ))) &&
#'   # (length(unique(
#'   #   object@Measurements$Step
#'   # )) == length(object@Steps_Timetrace)) &&
#'   # length(object@Steps_AVG) != 0) ||
#'   # (!(length(unique(
#'   #   object@Measurements$Step
#'   # )) == dim(object@Stimulus)[1])) &&
#'   # length(unique(object@Measurements$Step)) != 0) {
#'   #   ret <- ret + 1
#'   #   stop("Stimulus table incompatible with items in Steps or Measurements")
#'   # }
#'   if (any(duplicated(object@Stimulus$Description))) {
#'     ret <- ret + 1
#'     stop(paste(
#'       "Descriptions provided not unique: ",
#'       object@Stimulus$Description[duplicated(object@Stimulus$Description)]
#'     ))
#'   }
#'   # if (!length(object@Steps_AVG) == 0) {
#'   #   if (!length(object@Steps_AVG) == length(object@Steps_Excl)) {
#'   #     ret <- ret + 1
#'   #     stop("List of Average Traces and Excluded Traces not compatible")
#'   #   }
#'   # }
#'
#'   # checks for raw data
#'   # if (length(object@Steps_RAW) != 0) {
#'   #   if (!inherits(object@Steps_RAW, "ERGSteps")) {
#'   #     return("Steps_RAW slot must be an object of class 'ERGSteps'")
#'   #   }
#'   # }
#'
#'
#'   # if (length(object@Steps_RAW) != 0 &&
#'   #     length(object@Steps_AVG) != 0) {
#'   #   if (!length(object@Steps_RAW) == length(object@Steps_AVG)) {
#'   #     ret <- ret + 1
#'   #     stop("List of Raw Traces and Average Traces not compatible")
#'   #   }
#'   # }
#'   #
#'   # if (length(object@Steps_RAW) != 0 &&
#'   #     length(unique(object@Measurements$Step)) != 0) {
#'   #   if (!length(object@Steps_RAW) == length(unique(object@Measurements$Step))) {
#'   #     ret <- ret + 1
#'   #     stop("List of Raw Traces and Stats not compatible")
#'   #   }
#'   # }
#'
#'   if (!(length(
#'     colnames(object@Stimulus) %in% c("Description", "Intensity", "Background", "Type") ==
#'     4
#'   ))) {
#'     stop("Stimulus table malformatted")
#'   }
#'
#'   if (object@ProtocolName == "") {
#'     stop("No Protocol Name provided")
#'   }
#'
#'   if (object@ExamDate > object@Imported) { # was: as.POSIXct(object@Imported))
#'     stop(paste("Exam date is in future:", as.character(object@ExamDate),"<", object@Imported ))
#'   }
#'
#'   if (object@DOB > as.POSIXct(object@ExamDate)) {
#'     stop("Exam date before DOB.")
#'   }
#'
#'   if (object@Filename == "") {
#'     stop("Filename not provided")
#'   }
#'
#'   if (object@Patient == "") {
#'     stop("Patient name not provided")
#'   }
#'
#'   if (ret == 0) {
#'     TRUE
#'   } else {
#'     FALSE
#'   }
#' }
#'
#' #' An S4 class storing an ERG Exam
#' #'
#' #' This object is designed to store data and metadata from a single ERG experiment/exam.
#' #'
#' #' @slot Assays A list reserved for storing the raw (repeated) measurements as \linkS4class{EPhysRAW objects}.
#' #' @slot Results A list reserved for storing processed (usually averaged) raw data.
#' #' @slot Measurements A data.frame containing results from measurements performed on ERG traces. Each row represents one measurement.
#' #' @slot Channels A character vector containing the names of the recorded channels.
#' #' @slot Eye A character vector specifying the eye (currently supported are "LE" or "RE").
#' #' @slot Stimulus A data.frame containing information on the stimuli employed. Each row represents one step.
#' #' @slot ProtocolName A character vector specifying the name of the protocol.
#' #' @slot Version A character vector specifying the version of the exam.
#' #' @slot ExamDate A POSIXct object representing the exam date and time.
#' #' @slot Filename A character vector specifying the filename of the exam data.
#' #' @slot RecMode A character vector specifying the recording mode.
#' #' @slot Patient A character vector specifying the patient's name.
#' #' @slot DOB A Date object representing the patient's date of birth.
#' #' @slot Gender A character vector specifying the patient's gender.
#' #' @slot Group A character vector specifying the patient's group.
#' #' @slot Investigator A character vector specifying the name of the investigator.
#' #' @slot Imported A POSIXct object representing the time/date when the data was imported.
#' #'
#' #' @seealso \linkS4class{ImportEpsion}
#' #' @exportClass ERGExam
#' ERGExam <- setClass(
#'   Class = "ERGExam",
#'   slots =  list(
#'     Assays = "list",
#'     Results = "list",
#'     Measurements = "data.frame",
#'     Channels = "character",
#'     Eye = "character",
#'     Stimulus = "data.frame",
#'     ProtocolName = "character",
#'     Version = "character",
#'     ExamDate = "POSIXct",
#'     Filename = "character",
#'     RecMode = "character",
#'     Patient = "character",
#'     DOB = "Date",
#'     Gender = "character",
#'     Group = "character",
#'     Investigator = "character",
#'     Imported = "POSIXct"
#'   ),
#'   validity = validERGExam
#' )
#'
#' #' @noMd
#' setMethod("show",
#'           "ERGExam",
#'           function(object) {
#'
#'             content <- character()
#'             if (nrow(object@Measurements) > 0) {
#'               content <- "Measurements"
#'             }
#'             if (length(object@Assays) > 0) {
#'               content <- paste0(content, ", Raw traces")
#'             }
#'             if (length(object@Results) > 0) {
#'               content <- paste0(content, ", Processed traces")
#'             }
#'
#'             cat("An object of class ERGExam.\n")
#'             cat("Protocol:", object@ProtocolName, ", Version:", object@Version, "\n")
#'             cat("From:", object@Patient, ", DOB:", as.character(object@DOB), ", Group:", as.character(object@Group), "\n")
#'             cat("Recorded:", as.character(object@ExamDate), "\n")
#'             cat("Steps:\n")
#'             print(kable(object@Stimulus))
#'             cat("\nChannels:\n\t", paste(object@Channels, collapse = "\n\t"))
#'             cat("\nContent:", content, "\n")
#'             cat("Source:", object@Filename)
#'           })
#'
#'
#' #' Create a new instance of ERGExam class
#' #'
#' #' This function creates a new instance of the "ERGExam" class, automatically
#' #' setting the "Imported" slot to the current date and time and checks the
#' #' validity of the final object using the \code{validERGExam} function.
#' #'
#' #' @param Assays A list containing assay information.
#' #' @param Results A list containing result information.
#' #' @param Measurements A data.frame containing measurement data.
#' #' @param Channels A character vector specifying the channels.
#' #' @param Eye A character vector specifying the eye (e.g., "Left" or "Right").
#' #' @param Stimulus A data.frame containing stimulus information.
#' #' @param ProtocolName A character vector specifying the protocol name.
#' #' @param Version A character vector specifying the version of the exam.
#' #' @param ExamDate A POSIXct object representing the exam date and time.
#' #' @param Filename A character vector specifying the filename of the exam data.
#' #' @param RecMode A character vector specifying the recording mode.
#' #' @param Patient A character vector specifying the patient's name.
#' #' @param DOB A Date object representing the patient's date of birth.
#' #' @param Gender A character vector specifying the patient's gender.
#' #' @param Group A character vector specifying the patient's group.
#' #' @param Investigator A character vector specifying the name of the investigator.
#' #'
#' #' @return An instance of the "ERGExam" class.
#' #' @export
#' #'
#' #' @examples
#' #' newERGExam(
#' #'   Assays = list(),
#' #'   Results = list(),
#' #'   Measurements = data.frame(),
#' #'   Channels = "Some channels",
#' #'   Eye = "Left",
#' #'   Stimulus = data.frame(),
#' #'   ProtocolName = "Some Protocol",
#' #'   Version = "1.0",
#' #'   ExamDate = as.POSIXct("2023-08-06 15:30:00", tz = "UTC"),
#' #'   Filename = "exam_data.txt",
#' #'   RecMode = "Mode",
#' #'   Patient = "John Doe",
#' #'   DOB = as.Date("1985-02-10"),
#' #'   Gender = "Male",
#' #'   Group = "Control",
#' #'   Investigator = "Dr. Smith"
#' #' )
#' newERGExam <- function(Assays, Results, Measurements, Channels, Eye,
#'                           Stimulus, ProtocolName, Version, ExamDate,
#'                           Filename, RecMode, Patient, DOB, Gender,
#'                           Group, Investigator) {
#'
#'   # Set Imported slot to current date and time
#'   Imported <- as.POSIXct(Sys.time())
#'
#'   # Create the ERGExam object
#'   new_exam <- new("ERGExam",
#'                   Assays = Assays,
#'                   Results = Results,
#'                   Measurements = Measurements,
#'                   Channels = Channels,
#'                   Eye = Eye,
#'                   Stimulus = Stimulus,
#'                   ProtocolName = ProtocolName,
#'                   Version = Version,
#'                   ExamDate = ExamDate,
#'                   Filename = Filename,
#'                   RecMode = RecMode,
#'                   Patient = Patient,
#'                   DOB = DOB,
#'                   Gender = Gender,
#'                   Group = Group,
#'                   Investigator = Investigator,
#'                   Imported = Imported)
#'
#'   # Check validity of the final object
#'   valid <- tryCatch(validERGExam(new_exam),
#'                     error = function(e) FALSE)
#'
#'   if (!valid) {
#'     stop("The created ERGExam object is not valid.")
#'   }
#'
#'   return(new_exam)
#' }
