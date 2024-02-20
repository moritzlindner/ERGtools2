#' @importFrom EPhysData Rejected FilterFunction AverageFunction
#' @importFrom stats var
validERGExam <- function(object) {
  # Check if the data frame has the required columns
  required_columns <- c("Step", "Eye", "Channel")
  if (!all(required_columns %in% names(object@Metadata))) {
    stop(
      "Metadata provided is not in correct format. Must be a data.frame with the columns 'Step', 'Eye', and 'Channel'."
    )
  }

  # Check if Eye entries valid
  if (!all(unique(object@Metadata$Eye) %in% c("RE", "LE","OD","OS","Unspecified","Both"))) {
    stop("Only 'RE', 'LE', 'OD', 'OS' 'Both' or 'Unspecified' are allowed as eye identifiers")
  }

  # Rejected (inEPhysRaw) - must be the same across all channels within one eye
  for (s in unique(object@Metadata$Step)) {
    for (e in unique(object@Metadata$Eye[object@Metadata$Step == s])) {
      ids.equal <-
        which(object@Metadata$Step == s & object@Metadata$Eye == e)
      feature.list <- lapply(object@Data[ids.equal], Rejected)
      feature.df <- tryCatch(
        as.data.frame(do.call(cbind, feature.list)),
        error = function(er) {
          stop(
            "'Rejected' slots are filled with vectors of unequal length in step ",
            s,
            "eye",
            e,
            "."
          )
        }
      )
      if (!all(apply(feature.df, 1, function(x) {
        if (length(x) > 1) {
          var(x) == 0
        } else{
          TRUE
        }
      }))) {
        warnings("'Rejected' vectors are not identical across channels in step ",
             s,
             " eye ",
             e,
             ".")
      }
    }
  }

  # filter.fx (inEPhysRaw) - must be the same across all eyes within one channel and step
  for (s in unique(object@Metadata$Step)) {
    for (c in unique(object@Metadata$Channel[object@Metadata$Step == s])) {
      ids.equal <-
        which(object@Metadata$Step == s &
                object@Metadata$Channel == c)
      feature.list <-
        lapply(object@Data[ids.equal], function(x) {
          deparse1(FilterFunction(x))
        })
      feature.df <- tryCatch(
        as.data.frame(do.call(cbind, feature.list)),
        error = function(e) {
          stop(
            "'filter.fx' slots are filled with vectors of unequal length in step ",
            s,
            "channel",
            c,
            "."
          )
        }
      )
      if (!all(apply(feature.df, 1, function(x) {
        length(unique(x)) == 1
      }))) {
        stop(
          "'filter.fx' vectors are not identical across both eyes in step ",
          s,
          "channel",
          c,
          "."
        )
      }
    }
  }

  # average.fx (inEPhysRaw) - must be the same across all eyes within one channel and step
  for (s in unique(object@Metadata$Step)) {
    for (c in unique(object@Metadata$Channel[object@Metadata$Step == s])) {
      ids.equal <-
        which(object@Metadata$Step == s &
                object@Metadata$Channel == c)
      feature.list <-
        lapply(object@Data[ids.equal], function(x) {
          deparse1(AverageFunction(x))
        })
      feature.df <- tryCatch(
        as.data.frame(do.call(cbind, feature.list)),
        error = function(e) {
          stop(
            "'average.fx' slots are filled with vectors of unequal length in step ",
            s,
            "channel",
            c,
            "."
          )
        }
      )
      if (!all(apply(feature.df, 1, function(x) {
        length(unique(x)) == 1
      }))) {
        stop(
          "'average.fx' vectors are not identical across both eyes in step ",
          s,
          "channel",
          c,
          "."
        )
      }
    }
  }

  # Averaged slot
  if (object@Averaged){
    Repeats<-unique(unlist(lapply(object@Data,function(x){dim(x)[2]})))
    if(length(Repeats)!=1 || Repeats != 1){
      warning("Object does not appear to contain averaged data. Multiple repeats observed.")
    }
  }

  # Measurements slot
  if (length(object@Measurements) != 0) {
    if (any(!("ERGMeasurements" %in% class(object@Measurements)),
            !validObject(object@Measurements))) {
      stop("'Measurements' slot must contain a valid ERGMeasurements object.")
    }

    # check if ChannelBinding matches Channel of Recording
    measurements<-Measurements(object@Measurements)
    for (r in unique(measurements$Recording)){
      cb<-unique(measurements$ChannelBinding[measurements$Recording ==r])
      if(length(cb)>1){
        stop("Measurements object malformed: multiple Channel Bindings for a single recording.")
      }
      if (Metadata(object)$Channel[r]!=cb){
        stop("ChannelBinding stored in Measurements slot does not match the channel of the respective recoding as stored in the parent object. Error orcurred for recording #", r,".")
      }
    }
  }

  # Stimulus slot
  if (!all(object@Stimulus$Step %in% unique(Metadata(object)$Step))) {
    stop("All stimuli described must correspond to a Step as defined in 'Metadata'.")
  }

  #Essential fields
  if (!(
    nzchar(object@SubjectInfo$Subject) &&
    !is.na(object@SubjectInfo$DOB) &&
    all(!is.na(object@ExamInfo$ExamDate))
  )) {
    stop("Subject Name, DOB and ExamDate must be provided.")
  }

  # Dates
  dob <- object@SubjectInfo$DOB
  exam_date <- object@ExamInfo$ExamDate
  imported_date <- object@Imported

  if (("Date" %in% class(dob)) &&
      ("POSIXct" %in% class(exam_date)) &&
      ("POSIXct" %in% class(imported_date))) {
  if (!(as.POSIXct(dob) < min(exam_date) && max(exam_date) < min(imported_date))) {
      stop("Temporal sequence of 'DOB', 'ExamDate', and 'Imported' is impossible. ")
    }
  } else {
    stop("DOB, ExamDate, and Imported should be of class Date ('DOB'), POSIXct.")
  }

  if(object@Imported<"2024-02-01"){
    message("This ERGExam object is outdated. Please run UpdateERGExam().")
  }
  TRUE
}

#' ERGExam Class
#'
#' A class representing an ERG (Electroretinogram) exam with associated data and attributes. This class extends the \link[EPhysData:EPhysSet]{EPhysData::EPhysSet} object and all methods valid for \link[EPhysData:EphysSet]{EPhysData::EphysSet} can also be applied to \code{ERGExam} objects.
#'
#' @slot Data Data A list of \link{EPhysData} objects. Each item containing a recording in respsonse to a particular Stimulus ("Step"), from a particular eye and data Channel (e.g. ERG or OP)-
#' @slot Metadata  A data frame containing metadata information associated with the data, each row corresponds to one item in \code{data}.
#' \describe{
#'   \item{Step}{A character vector containing the step name. A step describes data recorded in response to the same type of stimulus.}
#'   \item{Eye}{A character vector. Possible values "RE" (right eye) and "LE" (left eye).}
#'   \item{Channel}{A character vector containing the channel name. This can be "ERG", "VEP" or "OP" for instance.}
#'   \item{Channel}{A numeric vector containing the indices of individual results contained in an ERG exam. E.g., if a recording to one identical stimulus is performed twice, these would be distinguished by different indices in the Result column. Warning: This is currently experimental}
#' }
#'
#' @slot Stimulus
#' A data frame containing stimulus information.
#' \describe{
#'   \item{Step}{A character vector containing the step the given stimulus is associated with.}
#'   \item{Description}{A character vector describing the stimulus.}
#'   \item{Intensity}{An integer vector representing the intensity of the stimulus.}
#'   \item{Background}{A character vector describing the adaptation state of the retina for that stimulus (DA or LA).}
#'   \item{Type}{A character vector describing the type of the stimulus (e.g. Flash or Flicker).}
#' }
#'
#' @slot Averaged
#' TRUE if the object contains averaged data, FALES indicates object contains raw traces.
#'
#' @slot Measurements
#' An object of class \linkS4class{ERGMeasurements}
#'
#' @slot ExamInfo
#' A list containing exam-related information.
#' \describe{
#'   \item{ProtocolName}{A character vector indicating the name of the protocol.}
#'   \item{Version}{Optional: A character vector indicating the version of the protocol.}
#'   \item{ExamDate}{A \code{POSIXct} The date of the exam.}
#'   \item{Filename}{Optional: The filename associated where exam raw data have been imported from.}
#'   \item{RecMode}{Optional: A character vector indicating the recording mode.}
#'   \item{Investigator}{Optional: A character vector indicating the name of the investigator conducting the exam.}
#' }
#'
#' @slot SubjectInfo
#' A list containing subject-related information.
#' \describe{
#'   \item{Subject}{A character vector indicating the name of the subject}
#'   \item{DOB}{A \code{Date} object indicating the date of birth of the subject}
#'   \item{Gender}{Optional: A character vector indicating the gender of the subject}
#'   \item{Group}{Optional: A character vector indicating the study group to which the subject belongs.}
#' }
#'
#' @slot Imported
#' A \code{POSIXct} timestamp indicating when the object was imported.
#'
#' @name ERGExam
#' @seealso \link[EPhysData:EPhysData]{EPhysData::EPhysData-package} \link[EPhysData:EPhysData]{EPhysData::EPhysData-class} \link[EPhysData:EPhysSet]{EPhysData::EPhysSet-class}
#' @importClassesFrom EPhysData EPhysData EPhysSet
#' @importFrom units as_units
#' @aliases ERGExam-class
#' @exportClass ERGExam
ERGExam <- setClass(
  "ERGExam",
  contains = "EPhysSet",
  slots = c(
    Data = "list",
    # Data is a list of EPhysData objects
    Metadata = "data.frame",
    # Metadata is a data frame
    Stimulus = "data.frame",
    # Stimulus is a data frame
    Averaged = "logical",
    # Averaged is a logical
    Measurements = "ERGMeasurements",
    # Measurements is a data frame
    ExamInfo = "list",
    # ExamInfo is a list
    SubjectInfo = "list",
    # SubjectInfo is a list
    Imported = "POSIXct"
  ),
  prototype = list(
    Data = list(NULL),
    Metadata = data.frame(
      Step = character(),
      Eye = character(),
      Channel = character(),
      stringsAsFactors = FALSE
    ),
    Stimulus = data.frame(
      Step = character(),
      Description = character(),
      Intensity = as_units(integer(), unitless),
      Background = character(),
      Type = character()
    ),
    Averaged = FALSE,
    Measurements =  new("ERGMeasurements"),
    ExamInfo = list(
      ProtocolName = character(),
      Version = character(),
      ExamDate = as.POSIXct(integer()),
      Filename = character(),
      RecMode = character(),
      Investigator = character()
    ),
    SubjectInfo = list(
      Subject = character(),
      DOB = as.Date(x = integer(0), origin = "1970-01-01"),
      Gender = character(),
      Group = character()
    ),
    Imported = as.POSIXct(integer())
  ),
  validity = validERGExam
)

#' Create an instance of the ERGExam class
#'
#' @description This function creates an instance of the \code{ERGExam} class
#' with the specified data and attributes.
#'
#' @param Data A list of \linkS4class{EPhysData::EPhysData} objects.
#' @param Metadata A data frame containing metadata information associated with the data, each row corresponds to one list item.
#' \describe{
#'   \item{Step}{A character vector containing the steps associated with the data.}
#'   \item{Eye}{A character vector containing the possible values "RE" (right eye) and "LE" (left eye).}
#'   \item{Channel}{A character vector containing the unique names of the third level of \code{Data}.}
#' }
#' @param Stimulus A data frame containing stimulus information associated with the data.
#' \describe{
#'   \item{Step}{A character vector specifying the steps associated with the stimulus.}
#'   \item{Name}{A character vector specifying the names of the stimuli.}
#'   \item{Description}{A character vector describing the stimuli.}
#'   \item{Adaptation}{A character vector describing the adaptation state of the stimulus.}
#'   \item{Background}{An integer vector representing the background intensity of the stimulus (unitless).}
#'   \item{Intensity}{An integer vector representing the intensity of the stimulus (unitless).}
#' }
#' @param Averaged A list of averaged data.
#' @param Measurements An object of class \linkS4class{ERGMeasurements}.
#' @param ExamInfo A list containing exam-related information.
#' \describe{
#'   \item{ProtocolName}{A character vector indicating the name of the protocol.}
#'   \item{Version}{Optional: A character vector indicating the version of the protocol.}
#'   \item{ExamDate}{A \code{POSIXct} timestamp representing the date of the exam.}
#'   \item{Filename}{Optional: A character vector indicating the filename associated with the exam data.}
#'   \item{RecMode}{Optional: A character vector indicating the recording mode during the exam.}
#'   \item{Investigator}{Optional: A character vector indicating the name of the investigator conducting the exam.}
#' }
#' @param SubjectInfo A list containing subject-related information.
#' \describe{
#'   \item{Subject}{A character vector indicating the name of the patient.}
#'   \item{DOB}{A \code{Date} object indicating the date of birth of the patient.}
#'   \item{Gender}{Optional: A character vector indicating the gender of the patient.}
#'   \item{Group}{Optional: A character vector indicating the group to which the patient belongs.}
#' }
#' @examples
#' # Create example data and metadata
#' Data <-
#'   list(
#'     makeExampleEPhysData(nrows = 10, replicate_count = 3),
#'     makeExampleEPhysData(nrows = 10, replicate_count = 3),
#'     makeExampleEPhysData(nrows = 5, replicate_count = 6),
#'     makeExampleEPhysData(nrows = 5, replicate_count = 6)
#'   )  # List of EPhysData objects
#' Metadata <-
#'   data.frame(
#'     Step = c("A1", "A1", "A2", "A2"),
#'     Eye = c("RE", "LE", "LE", "LE"),
#'     Channel = c("Ch1", "Ch1", "Ch1", "Ch2")
#'   )
#' Stimulus <-
#'   data.frame(Step = c("A1", "A2"),
#'              Name = c("Stim1", "Stim2"))  # Example stimulus data
#' ExamInfo <-
#'   list(
#'     ProtocolName = "ERG Protocol",
#'     Version = "1.0",
#'     ExamDate = as.POSIXct("2023-08-14"),
#'     Filename = "exam_data.csv"
#'   )
#' SubjectInfo <-
#'   list(
#'     Subject = "John Doe",
#'     DOB = as.Date("1990-05-15"),
#'     Gender = "Male",
#'     Group = "Control"
#'   )
#' ergExam <-
#'   newERGExam(
#'     Data = Data,
#'     Metadata = Metadata,
#'     Stimulus = Stimulus,
#'     ExamInfo = ExamInfo,
#'     SubjectInfo = SubjectInfo
#'   )
#'
#' @return An object of class \code{ERGExam}.
#' @seealso \linkS4class{ERGExam}
#' @importFrom units as_units
#' @importFrom methods new validObject
#' @export
newERGExam <-
  function(Data,
           Metadata,
           Stimulus,
           Averaged = FALSE,
           Measurements =  new("ERGMeasurements"),
           ExamInfo,
           SubjectInfo) {
    # Call the default constructor
    obj <- new("ERGExam")

    # Set the values for the slots
    obj@Data <- Data
    obj@Metadata <- Metadata
    obj@Stimulus <- Stimulus
    obj@Averaged <- Averaged
    obj@Measurements <- Measurements
    obj@ExamInfo <- ExamInfo
    obj@SubjectInfo <- SubjectInfo

    # Set default values for other slots
    obj@Imported <- as.POSIXct(Sys.time())

    # Call the validity method to check if the object is valid
    if (validObject(obj)) {
      return(obj)
    }
  }

#' @noMd
setAs("EPhysSet", "ERGExam", function(from) {
  new(to, Data = from@Data, Metadata = Metadata(from))
})

#' @noMd
#' @importFrom crayon green yellow
#' @importFrom utils object.size
setMethod("show",
          "ERGExam",
          function(object) {
            cat("An object of class ERGExam")
            cat(
              green("\nSubject:\t"),
              green(Subject(object)),
              ", ",
              as.character(DOB(object)),
              ", ",
              object@SubjectInfo$Gender,
              sep = ""
            )
            cat(green("\nExam Date:\t", as.character(ExamDate(object))))
            cat("\nProtocol:\t", object@ExamInfo$ProtocolName)
            cat("\nSteps:\t\t")
            cat(object@Stimulus$Description, sep = "\n\t\t")
            cat("Eyes:\t", Eyes(object), sep = "\t")
            cat("\nChannels:\t")
            cat(Channels(object), sep = "\n\t\t")
            if (length(object@Data) == 0) {
              cat("\nRaw data not stored in object.")
            }
            if (object@Averaged) {
              cat("\nObject contains imported averaged traces.")
            }
            if (length(object@Measurements) == 0) {
              cat("\nNo measurements stored in object.")
            } else {
              if (object@Averaged) {
                cat(green("\n*Measurements imported from external source.*"))
              }
            }
            if(!CheckAvgFxSet(object)){
              cat(yellow("\nAn averge function has not been set for this object."))
            }else{
              cat("\nAn averge function has been set for this object.")
            }
            cat("\nSize:", format(object.size(object), "auto"),"\n")
          })
